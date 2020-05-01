#include "parser.h"
#include "ast.h"
#include "fmt/core.h"
#include <cassert>
#include <regex>

namespace cmp {

template <typename T> using Res = ParserResult<T>;

Parser::Parser(Lexer &l, std::vector<Error> &e, std::vector<Error> &b)
    : lexer{l}, errors(e), beacons(b) {
  tok = lexer.lex();
  // insert keywords in name table
  for (auto m : keyword_map)
    names.getOrAdd(std::string{m.first});
}

void Parser::error(const std::string &msg) {
    errors.push_back({locate(), msg});
}

void Parser::errorExpected(const std::string &msg) {
    std::string s = fmt::format("expected {}, found '{}'", msg, tok.str());
    error(s);
}

void Parser::next() {
  if (tok.kind == Tok::eos)
    return;

  tok = lexer.lex();

  // If an error beacon is found in a comment, add the error to the parser
  // error list so that it can be compared to the actual errors later in the
  // verifying phase.
  if (tok.kind == Tok::comment) {
    std::string_view marker{"// ERROR: "};
    auto found = tok.text.find(marker);
    if (found != 0)
      return;

    // '---' in '// ERROR: ---'
    std::string regex_string{tok.text.substr(marker.length())};
    beacons.push_back({locate(), regex_string});
  }
}

// Returns true if match succeeded, false otherwise.
bool Parser::expect(Tok kind, const std::string &msg = "") {
    if (tok.kind != kind) {
        std::string s = msg;
        if (msg.empty())
            s = fmt::format("expected '{}', found '{}'",
                            tokenTypeToString(kind), tok.text);
        error(s);
        // Don't make progress if the match failed.
        // Note: the Go compiler does otherwise. Is that necessary?
        return false;
    }
    next();
    return true;
}

// Assumes that comments can only come at the end of a line, i.e. it considers
// only the line '//' comments.
bool Parser::is_end_of_stmt() const {
    return tok.kind == Tok::newline || tok.kind == Tok::comment;
}

bool Parser::is_eos() {
    skip_newlines();
    return tok.kind == Tok::eos;
}

// Parse a statement.
//
// Stmt:
//     Decl
//     Expr
Stmt *Parser::parseStmt() {
  Stmt *stmt = nullptr;

  if (tok.kind == Tok::lbrace) {
    stmt = parseCompoundStmt();
  } else if (tok.kind == Tok::kw_return) {
    stmt = parse_return_stmt();
  } else if (tok.kind == Tok::kw_if) {
    stmt = parse_if_stmt();
  } else if (tok.kind == Tok::hash) {
    stmt = parseBuiltinStmt();
  } else if (isStartOfDecl()) {
    stmt = parseDeclStmt();
  } else {
    stmt = parse_expr_or_assign_stmt();
  }
  skip_newlines();

  return stmt;
}

Stmt *Parser::parse_return_stmt() {
    auto pos = tok.pos;

    expect(Tok::kw_return);

    // optional
    Expr *expr = nullptr;
    if (!is_end_of_stmt()) {
        expr = parse_expr();
    }
    if (!is_end_of_stmt()) {
        skip_until_end_of_line();
        expect(Tok::newline);
        return make_node_pos<BadStmt>(pos);
    }
    skip_until_end_of_line();
    expect(Tok::newline);
    return make_node_pos<ReturnStmt>(pos, expr);
}

// Simplest way to represent the if-elseif-else chain is to view the else-if
// clause as simply a separate if statement that is embedded under the else
// statement.
IfStmt *Parser::parse_if_stmt() {
    auto pos = tok.pos;

    expect(Tok::kw_if);

    Expr *cond = parse_expr();
    CompoundStmt *cstmt = parseCompoundStmt();

    IfStmt *elseif = nullptr;
    CompoundStmt *cstmt_false = nullptr;

    if (tok.kind == Tok::kw_else) {
        next();

        if (tok.kind == Tok::kw_if) {
            elseif = parse_if_stmt();
        } else if (tok.kind == Tok::lbrace) {
            cstmt_false = parseCompoundStmt();
        } else {
            expect(Tok::lbrace);

            // do our best to recover
            parse_expr();
            if (tok.kind == Tok::lbrace) {
                cstmt_false = parseCompoundStmt();
            } else {
                skip_to_next_line();
            }
        }
    }

    return make_node_pos<IfStmt>(pos, cond, cstmt, elseif, cstmt_false);
}

// Parse 'let a = ...'
DeclStmt *Parser::parseDeclStmt() {
  auto decl = parseDecl();
  if (!is_end_of_stmt()) {
    // XXX: remove bad check
    if (decl && decl->kind != DeclNodeKind::bad)
      expect(Tok::newline);
    // try to recover
    skip_until_end_of_line();
  }
  return make_node<DeclStmt>(decl);
}

// Upon seeing an expression, we don't know yet if it is a simple expression
// statement or an assignment statement until we see the '=' token and the RHS.
// This function handles both cases in one go.
//
// TODO: function call?
Stmt *Parser::parse_expr_or_assign_stmt() {
    auto pos = tok.pos;

    auto lhs = parse_expr();
    // ExprStmt: expression ends with a newline
    if (is_end_of_stmt()) {
        skip_until_end_of_line();
        expect(Tok::newline);
        return make_node<ExprStmt>(lhs);
    }

    // AssignStmt: expression is followed by equals
    // (anything else is treated as an error)
    if (!expect(Tok::equals)) {
        skip_until_end_of_line();
        expect(Tok::newline);
        return make_node_pos<BadStmt>(pos);
    }

    // At this point, it becomes certain that this is an assignment statement,
    // and so we can safely unwrap for RHS.
    auto rhs = parse_expr();
    return make_node_pos<AssignStmt>(pos, lhs, rhs);
}

// Compound statement is a scoped block that consists of multiple statements.
// There is no restriction in order such as variable declarations should come
// first, etc.
//
// CompoundStmt:
//     { Stmt* }
CompoundStmt *Parser::parseCompoundStmt() {
    expect(Tok::lbrace);
    auto compound = make_node<CompoundStmt>();

    while (true) {
        skip_newlines();
        if (tok.kind == Tok::rbrace)
            break;
        auto stmt = parseStmt();
        compound->stmts.push_back(stmt);
    }

    expect(Tok::rbrace);
    return compound;
}

BuiltinStmt *Parser::parseBuiltinStmt() {
  auto start = tok.pos;
  skip_until_end_of_line();
  auto end = tok.pos;
  std::string_view text{lexer.source().buf.data() + start, end - start};
  return make_node_pos<BuiltinStmt>(start, text);
}

// Doesn't include 'let' or 'var'.
VarDeclNode *Parser::parseVarDecl(VarDeclNodeKind kind) {
  auto pos = tok.pos;

  Name *name = names.getOrAdd(std::string{tok.text});
  next();

  VarDeclNode *v = nullptr;
  // '=' comes either first, or after the ': type' part.
  if (tok.kind == Tok::colon) {
    next();
    auto type_expr = parseTypeExpr();
    v = make_node_pos<VarDeclNode>(pos, name, kind, type_expr, nullptr);
  }
  if (tok.kind == Tok::equals) {
    next();
    auto assign_expr = parse_expr();
    if (v)
      static_cast<VarDeclNode *>(v)->assign_expr = assign_expr;
    else
      v = make_node_pos<VarDeclNode>(pos, name, kind, nullptr, assign_expr);
  }
  if (!v) {
    errorExpected("'=' or ':' after var name");
    v = nullptr;
  }
  return v;
}

// Parses a comma separated list of AST nodes whose type is T*.  Parser
// function for the node type should be provided as 'parseFn' so that this
// function knows how to parse the elements.
// Doesn't parse the enclosing parentheses or braces.
template <typename T, typename F>
std::vector<T *> Parser::parseCommaSeparatedList(F &&parseFn) {
  std::vector<T *> list;
  auto finishers = {Tok::rparen, Tok::rbrace};
  auto delimiters = {Tok::comma, Tok::newline, Tok::rparen, Tok::rbrace};

  while (true) {
    skip_newlines();
    if (tok.isAny(finishers))
      break;

    auto elem = parseFn();
    list.push_back(elem);

    // Determining where each decl ends in a list is a little tricky.  Here, we
    // stop for any token that is either (1) separator tokens, i.e. comma,
    // newline, or (2) used to enclose a decl list, i.e.  parentheses and
    // braces.  This works for both function argument lists and struct member
    // lists.
    if (!elem) {
      skip_until_any(delimiters);
    } else if (!tok.isAny(delimiters)) {
      // For cases when a VarDecl succeeds parsing but there is a leftover
      // token, e.g. 'a: int#', we need to directly check the next token is the
      // delimiting token, and do an appropriate error report.
      error(fmt::format("trailing token '{}' after declaration", tok.str()));
      skip_until_any(delimiters);
    }

    // skip comma if any
    if (tok.kind == Tok::comma)
      next();
  }

  return list;
}

FuncDeclNode *Parser::parseFuncDecl() {
  auto pos = tok.pos;

  expect(Tok::kw_func);

  Name *name = names.getOrAdd(std::string{tok.text});
  auto func = make_node<FuncDeclNode>(name);
  func->pos = tok.pos;
  next();

  // argument list
  expect(Tok::lparen);
  func->args = parseCommaSeparatedList<VarDeclNode>(
      [this] { return parseVarDecl(VarDeclNodeKind::param); });
  if (!expect(Tok::rparen)) {
    skip_until(Tok::rparen);
    expect(Tok::rparen);
  }

  // return type (-> ...)
  if (tok.kind == Tok::arrow) {
    next();
    func->retTypeExpr = parseTypeExpr();
  }

  if (tok.kind != Tok::lbrace) {
    errorExpected("'->' or '{'");
    skip_until(Tok::lbrace);
  }

  // function body
  func->body = parseCompoundStmt();
  func->pos = pos;

  return func;
}

StructDeclNode *Parser::parseStructDecl() {
  auto pos = tok.pos;
  Name *name = nullptr;

  expect(Tok::kw_struct);

  if (tok.kind != Tok::ident) {
    errorExpected("an identifier");
    skip_until(Tok::lbrace);
  } else {
    name = names.getOrAdd(std::string{tok.text});
    next();
  }

  if (!expect(Tok::lbrace))
    skip_until_end_of_line();
  auto fields = parseCommaSeparatedList<VarDeclNode>(
      [this] { return parseVarDecl(VarDeclNodeKind::struct_); });
  expect(Tok::rbrace, "unterminated struct declaration");
  // TODO: recover

  return make_node_pos<StructDeclNode>(pos, name, fields);
}

EnumVariantDeclNode *Parser::parseEnumVariant() {
  auto pos = tok.pos;

  Name *name = names.getOrAdd(std::string{tok.text});
  fmt::print("EnumVariant: '{}'\n", name->str());
  next();

  std::vector<Expr *> fields;
  if (tok.kind == Tok::lparen) {
    expect(Tok::lparen);
    fields = parseCommaSeparatedList<Expr>([this] { return parseTypeExpr(); });
    expect(Tok::rparen);
  }

  return make_node_pos<EnumVariantDeclNode>(pos, name, fields);
}

// Doesn't account for the enclosing {}s.
std::vector<EnumVariantDeclNode *> Parser::parseEnumVariantDeclList() {
  std::vector<EnumVariantDeclNode *> list;

  while (true) {
    skip_newlines();
    if (tok.kind != Tok::ident)
      break;

    auto elem = parseEnumVariant();
    list.push_back(elem);

    expect(Tok::newline);
    skip_newlines();
  }

  return list;
}

EnumDeclNode *Parser::parseEnumDecl() {
  fmt::print("Hi I'm here!\n");
  auto pos = tok.pos;

  expect(Tok::kw_enum);

  if (tok.kind != Tok::ident)
    errorExpected("an identifier");
  Name *name = names.getOrAdd(std::string{tok.text});
  next();

  if (!expect(Tok::lbrace))
    skip_until_end_of_line();
  auto fields = parseEnumVariantDeclList();
  expect(Tok::rbrace, "unterminated enum declaration");
  // TODO: recover

  return make_node_pos<EnumDeclNode>(pos, name, fields);
}

bool Parser::isStartOfDecl() const {
    switch (tok.kind) {
    case Tok::kw_let:
    case Tok::kw_var:
        return true;
    default:
        return false;
    }
}

// 'let a = ...'
DeclNode *Parser::parseDecl() {
    switch (tok.kind) {
    case Tok::kw_let:
        next();
        return parseVarDecl(VarDeclNodeKind::local);
    // TODO: 'var'
    default:
        assert(false && "not a start of a declaration");
    }
    // unreachable
    return nullptr;
}

Expr *Parser::parseLiteralExpr() {
    Expr *expr = nullptr;
    // TODO Literals other than integers?
    switch (tok.kind) {
    case Tok::number: {
        std::string s{tok.text};
        int value = std::stoi(s);
        expr = make_node<IntegerLiteral>(value);
        break;
    }
    case Tok::string:
        expr = make_node<StringLiteral>(tok.text);
        break;
    default:
        assert(false && "non-integer literals not implemented");
    }
    expr->pos = tok.pos;

    next();

    return expr;
}

// Upon seeing an expression that starts with an identifier, we don't know
// whether it is just a variable, a function call, or struct initialization
// without lookahead ('a' vs 'a()' vs 'a {...}'). Rather than using lookahead,
// parse the both kinds in one go in this function.
//
// TODO: maybe name it parse_ident_start_exprs?
// TODO: add struct declaration here, e.g. Car {}
Expr *Parser::parseFuncCallOrDeclRefExpr() {
    auto pos = tok.pos;
    assert(tok.kind == Tok::ident);
    auto name = names.getOrAdd(std::string{tok.text});
    next();

    if (tok.kind == Tok::lparen) {
        expect(Tok::lparen);
        std::vector<Expr *> args;
        while (tok.kind != Tok::rparen) {
            args.push_back(parse_expr());
            if (tok.kind == Tok::comma)
                next();
        }
        expect(Tok::rparen);
        return make_node_pos<FuncCallExpr>(pos, name, args);
    } else {
        return make_node_pos<DeclRefExpr>(pos, name);
    }
}

bool Parser::isStartOfTypeExpr() const {
  return tok.kind == Tok::star || isIdentOrKeyword(tok);
}

// Parse a type expression.
// A type expression is simply every stream of tokens in the source that can
// represent a type.
//
// TypeExpr:
//     '&' TypeExpr
//     'mut'? ident
Expr *Parser::parseTypeExpr() {
  auto pos = tok.pos;
  bool mut = false;
  TypeExprKind type_kind = TypeExprKind::value;
  Expr *subexpr = nullptr;

  // XXX OUTDATED: Encode each type into a unique Name, so that they are easy
  // to find in the type table in the semantic analysis phase.
  std::string text;
  if (tok.kind == Tok::star) {
    next();
    type_kind = TypeExprKind::ref;
    subexpr = parseTypeExpr();
    if (subexpr->kind == ExprKind::type) {
      text = "*" + subexpr->as<TypeExpr>()->name->text;
    }
  } else if (isIdentOrKeyword(tok)) {
    if (tok.kind == Tok::kw_mut) {
      expect(Tok::kw_mut);
      mut = true;
    }
    if (!isIdentOrKeyword(tok)) {
      // FIXME: type name? expression?
      errorExpected("type name");
      return make_node_pos<BadExpr>(pos);
    }
    type_kind = TypeExprKind::value;
    subexpr = nullptr;
    text = tok.text;
    next();
  } else {
    errorExpected("type expression");
    return make_node_pos<BadExpr>(pos);
  }

  Name *name = names.getOrAdd(text);

  return make_node_pos<TypeExpr>(pos, type_kind, name, subexpr);
}

Expr *Parser::parseUnaryExpr() {
    auto pos = tok.pos;

    switch (tok.kind) {
    case Tok::number:
    case Tok::string:
        return parseLiteralExpr();
    case Tok::ident: {
        // TODO: do proper op precedence parsing
        auto expr = parseFuncCallOrDeclRefExpr();
        return parseMemberExprMaybe(expr);
    }
    case Tok::star: {
        next();
        auto expr = parseUnaryExpr();
        return make_node_pos<UnaryExpr>(pos, UnaryExprKind::deref, expr);
    }
    case Tok::ampersand: {
        next();
        auto expr = parseUnaryExpr();
        return make_node_pos<UnaryExpr>(pos, UnaryExprKind::address, expr);
    }
    case Tok::lparen: {
        expect(Tok::lparen);
        auto inside_expr = parse_expr();
        expect(Tok::rparen);
        return make_node_pos<ParenExpr>(pos, inside_expr);
    }
    // TODO: prefix (++), postfix, sign (+/-)
    default:
        // Because all expressions start with a unary expression, failing here
        // means no other expression could be matched either, so just do a
        // really generic report.
        errorExpected("an expression");
        return make_node_pos<BadExpr>(pos);
    }
}

static int binary_op_precedence(const Token &op) {
    switch (op.kind) {
    case Tok::star:
    case Tok::slash:
        return 2;
    case Tok::plus:
    case Tok::minus:
        return 1;
    case Tok::greaterthan:
    case Tok::lesserthan:
        return 0;
    default:
        // not an operator
        return -1;
    }
}

// Extend a unary expression into binary if possible, by parsing any attached
// RHS. There may be more than one terms in the RHS, all of which is consumed
// by this function. The parsing goes on as long as operators with higher than
// or equal to 'precedence' are seen. Giving this parameter 0 means to parse
// the whole chain of binary expressions.
Expr *Parser::parseBinaryExprRhs(Expr *lhs, int precedence = 0) {
    Expr *root = lhs;

    while (true) {
        int this_prec = binary_op_precedence(tok);

        // If the upcoming op has lower precedence, finish this subexpression.
        // It will be treated as a single term when this function is re-called
        // with lower precedence.
        if (this_prec < precedence) {
            return root;
        }

        Token op = tok;
        next();

        // Parse the second term.
        Expr *rhs = parseUnaryExpr();

        // We do not know if this term should associate to left or right;
        // e.g. "(a * b) + c" or "a + (b * c)".  We should look ahead for the
        // next operator that follows this term.
        int next_prec = binary_op_precedence(tok);

        // If the next operator has higher precedence ("a + b * c"), evaluate
        // the RHS as a single subexpression with elevated minimum precedence.
        // Else ("a * b + c"), just treat it as a unary expression.
        if (this_prec < next_prec) {
            rhs = parseBinaryExprRhs(rhs, precedence + 1);
        }

        // Create a new root with the old root as its LHS, and the recursion
        // result as RHS.  This implements left associativity.
        root = make_node<BinaryExpr>(root, op, rhs);
    }

    return root;
}

// If this expression is a member expression with a dot (.) operator, parse as
// such. If not, just pass along the original expression. This function is
// called after the operand expression part is fully parsed.
Expr *Parser::parseMemberExprMaybe(Expr *expr) {
    Expr *result = expr;

    while (tok.kind == Tok::dot) {
        expect(Tok::dot);

        Name *member_name = names.getOrAdd(std::string{tok.text});
        next();

        result = make_node_pos<MemberExpr>(result->pos, result, member_name);
    }

    return result;
}

Expr *Parser::parse_expr() {
    auto unary = parseUnaryExpr();
    if (!unary)
        return nullptr;
    auto binary = parseBinaryExprRhs(unary);
    return parseMemberExprMaybe(binary);
}

std::vector<Error> Parser::parse_error_beacon() {
    expect(Tok::lbracket);
    expect(Tok::kw_error);
    expect(Tok::colon);

    std::vector<Error> v;
    v.push_back({locate(), std::string{tok.text}});
    next();

    expect(Tok::rbracket);
    return v;
}

void Parser::skip_until(Tok kind) {
  while (tok.kind != kind)
    next();
}

void Parser::skip_until_any(const std::vector<Tok> &kinds) {
  while (!tok.isAny(kinds))
    next();
}

void Parser::skip_until_end_of_line() {
    while (tok.kind != Tok::newline)
        next();
}

// Used when the current statement turns out to be broken and we just want to
// skip this whole line.
void Parser::skip_to_next_line() {
    skip_until_end_of_line();
    expect(Tok::newline);
}

// The language is newline-aware, but newlines are mostly meaningless unless
// they are at the end of a statement or a declaration.  In those cases we use
// this to skip over them.
void Parser::skip_newlines() {
    while (tok.kind == Tok::newline || tok.kind == Tok::comment)
        next();
}

AstNode *Parser::parseToplevel() {
    skip_newlines();

    switch (tok.kind) {
    case Tok::kw_func:
        return parseFuncDecl();
    case Tok::kw_struct:
        return parseStructDecl();
    case Tok::kw_enum:
        return parseEnumDecl();
    default:
        error("assertion failed here");
        assert(false && "unreachable");
    }
}

File *Parser::parseFile() {
    auto file = make_node<File>();
    // FIXME
    while (!is_eos()) {
        auto toplevel = parseToplevel();
        if (!toplevel)
          continue;
        file->toplevels.push_back(toplevel);
    }
    return file;
}

Ast Parser::parse() {
    ast = parseFile();
    return Ast{ast, names};
}

} // namespace cmp
