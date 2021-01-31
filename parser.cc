#include "parser.h"
#include "ast.h"
#include "fmt/core.h"
#include <cassert>

namespace cmp {

Parser::Parser(Lexer &l, Sema &sema) : lexer{l}, sema(sema) {
    // insert keywords in name table
    for (auto m : keyword_map)
        sema.name_table.push(m.first);

    // set up lookahead and cache
    next();
}

void Parser::error(const std::string &msg) {
    auto srcloc = locate();
    sema.errors.push_back({srcloc, msg});
    fmt::print(stderr, "{}:{}:{}: {}\n", srcloc.filename, srcloc.line, srcloc.col, msg);
    exit(EXIT_FAILURE);
}

void Parser::error_expected(const std::string &msg) {
    std::string s = fmt::format("expected {}, found '{}'", msg, tok.str());
    error(s);
}

void Parser::next() {
    if (tok.kind == Tok::eos)
        return;

    // update cache if necessary
    if (next_read_pos == token_cache.size()) {
        auto t = lexer.lex();
        token_cache.push_back(t);
    }

    last_tok_endpos = tok.endPos();
    tok = token_cache[next_read_pos];
    next_read_pos++;
}

Parser::State Parser::save_state() {
    return State{tok, last_tok_endpos, next_read_pos, sema.errors.size()};
}

void Parser::restore_state(State state) {
    tok = state.tok;
    last_tok_endpos = state.last_tok_endpos;
    next_read_pos = state.next_read_pos;
    sema.errors.resize(state.error_count);
}

// Returns true if match succeeded, false otherwise.
bool Parser::expect(Tok kind, const std::string &msg = "") {
    if (tok.kind != kind) {
        std::string s = msg;
        if (msg.empty()) {
            s = fmt::format(
                "expected '{}', found '{}'", tokenTypeToString(kind),
                std::string{tok.start,
                            static_cast<size_t>(tok.end - tok.start)});
        }
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

bool Parser::is_eos() const { return tok.kind == Tok::eos; }

// Parse a statement.
//
// Stmt:
//     Decl
//     Expr
Stmt *Parser::parse_stmt() {
    Stmt *stmt = nullptr;

    if (tok.kind == Tok::lbrace) {
        stmt = parse_compound_stmt();
    } else if (tok.kind == Tok::kw_return) {
        stmt = parse_return_stmt();
    } else if (tok.kind == Tok::kw_if) {
        stmt = parse_if_stmt();
    } else if (tok.kind == Tok::hash) {
        stmt = parse_builtin_stmt();
    } else if (is_start_of_decl()) {
        stmt = parse_decl_stmt();
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
        return sema.make_node_pos<BadStmt>(pos);
    }
    skip_until_end_of_line();
    expect(Tok::newline);
    return sema.make_node_pos<ReturnStmt>(pos, expr);
}

// Simplest way to represent the if-elseif-else chain is to view the else-if
// clause as simply a separate if statement that is embedded under the else
// statement.
IfStmt *Parser::parse_if_stmt() {
    auto pos = tok.pos;

    expect(Tok::kw_if);

    Expr *cond = parse_expr();
    CompoundStmt *cstmt = parse_compound_stmt();

    IfStmt *elseif = nullptr;
    CompoundStmt *cstmt_false = nullptr;

    if (tok.kind == Tok::kw_else) {
        next();

        if (tok.kind == Tok::kw_if) {
            elseif = parse_if_stmt();
        } else if (tok.kind == Tok::lbrace) {
            cstmt_false = parse_compound_stmt();
        } else {
            expect(Tok::lbrace);

            // do our best to recover
            parse_expr();
            if (tok.kind == Tok::lbrace) {
                cstmt_false = parse_compound_stmt();
            } else {
                skip_to_next_line();
            }
        }
    }

    return sema.make_node_pos<IfStmt>(pos, cond, cstmt, elseif, cstmt_false);
}

// Parse 'let a = ...'
DeclStmt *Parser::parse_decl_stmt() {
    auto decl = parseDecl();
    if (!is_end_of_stmt()) {
        // XXX: remove bad check
        if (decl && !decl->is<BadDecl>()) {
            expect(Tok::newline);
        }
        // try to recover
        skip_until_end_of_line();
    }
    return sema.make_node<DeclStmt>(decl);
}

// Upon seeing an expression, we don't know yet if it is a simple expression
// statement or an assignment statement until we see the '=' token and the RHS.
// This function handles both cases in one go.
Stmt *Parser::parse_expr_or_assign_stmt() {
    auto pos = tok.pos;

    auto lhs = parse_expr();
    // ExprStmt: expression ends with a newline
    if (is_end_of_stmt()) {
        skip_until_end_of_line();
        expect(Tok::newline);
        return sema.make_node<ExprStmt>(lhs);
    }

    bool move = false;
    // AssignStmt: expression is followed by '=' or '<-'
    // (anything else is treated as an error)
    if (tok.kind == Tok::reversearrow) {
        move = true;
        next();
    } else if (!expect(Tok::equals, "expected '=' or '\\n' after expression")) {
        skip_until_end_of_line();
        expect(Tok::newline);
        return sema.make_node_pos<BadStmt>(pos);
    }

    // At this point, it becomes certain that this is an assignment statement,
    // and so we can safely unwrap for RHS.
    auto rhs = parse_expr();
    return sema.make_node_pos<AssignStmt>(pos, lhs, rhs, move);
}

// Compound statement is a scoped block that consists of multiple statements.
// There is no restriction in order such as variable declarations should come
// first, etc.
//
// CompoundStmt:
//     { Stmt* }
CompoundStmt *Parser::parse_compound_stmt() {
    expect(Tok::lbrace);
    auto compound = sema.make_node<CompoundStmt>();

    while (!is_eos()) {
        skip_newlines();
        if (tok.kind == Tok::rbrace)
            break;
        auto stmt = parse_stmt();
        compound->stmts.push_back(stmt);
    }

    expect(Tok::rbrace);
    return compound;
}

BuiltinStmt *Parser::parse_builtin_stmt() {
    auto start = tok.pos;
    skip_until_end_of_line();
    auto end = tok.pos;
    std::string_view text{lexer.source().buf.data() + start, end - start};
    return sema.make_node_pos<BuiltinStmt>(start, text);
}

static Name *push_token(Sema &sema, const Token tok) {
    return sema.name_table.pushlen(tok.start, tok.end - tok.start);
}

// Doesn't include 'let' or 'var'.
VarDecl *Parser::parse_var_decl(VarDeclKind kind) {
    auto pos = tok.pos;

    if (tok.kind != Tok::ident) {
        error_expected("an identifier");
    }

    Name *name = push_token(sema, tok);
    next();

    VarDecl *v = nullptr;
    // '=' comes either first, or after the ': type' part.
    if (tok.kind == Tok::colon) {
        next();
        auto type_expr = parse_type_expr();
        v = sema.make_node_pos<VarDecl>(pos, name, kind, type_expr, nullptr);
    }
    if (tok.kind == Tok::equals) {
        next();
        auto assign_expr = parse_expr();
        if (v)
            static_cast<VarDecl *>(v)->assign_expr = assign_expr;
        else
            v = sema.make_node_pos<VarDecl>(pos, name, kind, nullptr,
                                            assign_expr);
    }
    if (!v) {
        error_expected("'=' or ':' after var name");
        v = nullptr;
    }
    return v;
}

// Parses a comma separated list of AST nodes whose type is T*.  Parser
// function for the node type should be provided as 'parse_fn' so that this
// function knows how to parse the elements.
// Doesn't account for the enclosing parentheses or braces.
template <typename T, typename F>
std::vector<T> Parser::parse_comma_separated_list(F &&parse_fn) {
    auto finishers = {Tok::rparen, Tok::rbrace};
    auto delimiters = {Tok::comma, Tok::newline, Tok::rparen, Tok::rbrace};
    std::vector<T> list;

    for (;;) {
        skip_newlines();
        if (tok.is_any(finishers) || is_eos())
            break;

        auto elem = parse_fn();
        if (elem)
            list.push_back(elem);

        // Determining where each decl ends in a list is a little tricky.  Here,
        // we stop for any token that is either (1) separator tokens, i.e.
        // comma, newline, or (2) used to enclose a decl list, i.e.  parentheses
        // and braces.  This works for both function argument lists and struct
        // member lists.
        if (!elem) {
            skip_until_any(delimiters);
        } else if (!tok.is_any(delimiters)) {
            // For cases when a VarDecl succeeds parsing but there is a leftover
            // token, e.g. 'a: int###', we need to directly check the next token
            // is the delimiting token, and do an appropriate error report.
            error(fmt::format("trailing token '{}' after declaration",
                              tok.str()));
            skip_until_any(delimiters);
        }

        // Skip comma if any. This allows trailing comma at the end, e.g.
        // '(a,)'.
        if (tok.kind == Tok::comma)
            next();
    }

    return list;
}

FuncDecl *Parser::parse_func_header() {
    auto pos = tok.pos;

    expect(Tok::kw_func);

    Name *name = push_token(sema, tok);
    auto func = sema.make_node_pos<FuncDecl>(pos, name);
    func->loc = sema.source.locate(tok.pos);
    next();

    // argument list
    expect(Tok::lparen);
    func->args = parse_comma_separated_list<VarDecl *>(
        [this] { return parse_var_decl(VarDeclKind::param); });
    if (!expect(Tok::rparen)) {
        skip_until(Tok::rparen);
        expect(Tok::rparen);
    }

    // return type (-> ...)
    if (tok.kind == Tok::arrow) {
        next();
        func->rettypeexpr = parse_type_expr();
    }

    return func;
}

FuncDecl *Parser::parse_func_decl() {
    auto func = parse_func_header();

    if (tok.kind != Tok::lbrace) {
        error_expected("'->' or '{'");
        skip_until(Tok::lbrace);
    }

    // function body
    func->body = parse_compound_stmt();

    return func;
}

StructDecl *Parser::parse_struct_decl() {
    auto pos = tok.pos;
    Name *name = nullptr;

    expect(Tok::kw_struct);

    if (tok.kind != Tok::ident) {
        error_expected("an identifier");
        skip_until(Tok::lbrace);
    } else {
        name = push_token(sema, tok);
        next();
    }

    if (!expect(Tok::lbrace))
        skip_until_end_of_line();

    auto fields = parse_comma_separated_list<VarDecl *>(
        [this] { return parse_var_decl(VarDeclKind::struct_); });
    expect(Tok::rbrace, "unterminated struct declaration");
    // TODO: recover

    return sema.make_node_pos<StructDecl>(pos, name, fields);
}

EnumVariantDecl *Parser::parse_enum_variant() {
    auto pos = tok.pos;

    Name *name = push_token(sema, tok);
    next();

    std::vector<Expr *> fields;
    if (tok.kind == Tok::lparen) {
        expect(Tok::lparen);
        fields = parse_comma_separated_list<Expr *>(
            [this] { return parse_type_expr(); });
        expect(Tok::rparen);
    }

    return sema.make_node_pos<EnumVariantDecl>(pos, name, fields);
}

// Doesn't account for the enclosing {}s.
std::vector<EnumVariantDecl *> Parser::parse_enum_variant_decl_list() {
    std::vector<EnumVariantDecl *> list;

    while (!is_eos()) {
        skip_newlines();
        if (tok.kind != Tok::ident)
            break;

        auto elem = parse_enum_variant();
        list.push_back(elem);

        expect(Tok::newline);
        skip_newlines();
    }

    return list;
}

EnumDecl *Parser::parse_enum_decl() {
    auto pos = tok.pos;

    expect(Tok::kw_enum);

    if (tok.kind != Tok::ident)
        error_expected("an identifier");
    Name *name = push_token(sema, tok);
    next();

    if (!expect(Tok::lbrace))
        skip_until_end_of_line();
    auto fields = parse_enum_variant_decl_list();
    expect(Tok::rbrace, "unterminated enum declaration");
    // TODO: recover

    return sema.make_node_pos<EnumDecl>(pos, name, fields);
}

ExternDecl *Parser::parse_extern_decl() {
    auto pos = tok.pos;
    expect(Tok::kw_extern);
    auto func = parse_func_header();
    return sema.make_node_pos<ExternDecl>(pos, func);
}

bool Parser::is_start_of_decl() {
    switch (tok.kind) {
    case Tok::kw_let:
    case Tok::kw_struct:
    case Tok::kw_func:
        return true;
    case Tok::kw_var: {
        // For var, there can be exceptions such as 'var &a'. We need to do some
        // lookahead here.
        auto s = save_state();
        next();
        if (tok.kind == Tok::ampersand) {
            restore_state(s);
            return false;
        }
        restore_state(s);
        return true;
    }
    default:
        return false;
    }
}

// Parse a declaration.
// Remember to modify is_start_of_decl() accordingly.
Decl *Parser::parseDecl() {
    assert(is_start_of_decl());

    switch (tok.kind) {
    case Tok::kw_let: {
        next();
        auto v = parse_var_decl(VarDeclKind::local);
        return v;
    }
    case Tok::kw_var: {
        next();
        auto v = parse_var_decl(VarDeclKind::local);
        v->mut = true;
        return v;
    }
    case Tok::kw_struct: {
        return parse_struct_decl();
    }
    case Tok::kw_func:
        return parse_func_decl();
    default: {
        assert(false && "not a start of a declaration");
    }
    }
}

Expr *Parser::parse_literal_expr() {
    Expr *expr = nullptr;
    // TODO Literals other than integers?
    switch (tok.kind) {
    case Tok::number: {
        std::string s{tok.start, static_cast<size_t>(tok.end - tok.start)};
        int value = std::stoi(s);
        expr = sema.make_node_range<IntegerLiteral>({tok.pos, tok.endPos()},
                                                    value);
        break;
    }
    case Tok::string:
        expr = sema.make_node_range<StringLiteral>(
            {tok.pos, tok.endPos()},
            std::string_view{tok.start,
                             static_cast<size_t>(tok.end - tok.start)});
        break;
    default:
        assert(false && "non-integer literals not implemented");
    }

    next();

    return expr;
}

// Upon seeing an expression that starts with an identifier, we don't know
// whether it is just a variable, a function call, an enum name, or a struct
// name ('a' vs 'a()' vs 'a {...}') without lookahead. Rather than using
// lookahead, parse the both kinds in one go in this function.
//
// TODO: maybe name it parse_ident_start_exprs?
// TODO: add struct declaration here, e.g. Car {}
Expr *Parser::parse_funccall_or_declref_expr() {
    auto pos = tok.pos;
    assert(tok.kind == Tok::ident);
    Name *name = push_token(sema, tok);
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
        return make_node_range<CallExpr>(pos, CallExprKind::func, name, args);
    } else {
        // Whether this is a variable or a struct/enum name can only be decided
        // in the type checking stage.
        return make_node_range<DeclRefExpr>(pos, name);
    }
}

Expr *Parser::parse_cast_expr() {
    auto pos = tok.pos;

    expect(Tok::lbracket);
    auto type_expr = parse_type_expr();
    expect(Tok::rbracket);

    expect(Tok::lparen);
    auto target_expr = parse_expr();
    expect(Tok::rparen);

    return make_node_range<CastExpr>(pos, type_expr, target_expr);
}

// Get the Name handle that designates a reference type of a given referee type
// name.  This function is used in the typeck phase to query already-declared
// reference types from the type table. TODO: looks kinda like an unnecessary
// step.
Name *name_of_derived_type(NameTable &names, TypeKind kind,
                           Name *referee_name) {
    std::string prefix;
    switch (kind) {
    case TypeKind::var_ref:
        prefix = "var &";
        break;
    case TypeKind::ref:
        prefix = "&";
        break;
    case TypeKind::ptr:
        prefix = "*";
        break;
    default:
        assert(false && "unreachable");
    }
    prefix += referee_name->text;
    return names.push(prefix.c_str());
}

// Parse a type expression.
// A type expression is simply every stream of tokens in the source that can
// represent a type.
//
// type-expression:
//     ('var'? '&')? ident
Expr *Parser::parse_type_expr() {
    auto pos = tok.pos;

    bool mut = false;
    if (tok.kind == Tok::kw_var) {
        next();
        mut = true;
    }

    TypeKind type_kind = TypeKind::value;
    Name *lt_name = nullptr;
    Expr *subexpr = nullptr;
    std::string text;
    if (tok.kind == Tok::ampersand) {
        expect(Tok::ampersand);
        type_kind = mut ? TypeKind::var_ref : TypeKind::ref;
        // Lifetime annotation.
        if (tok.kind == Tok::dot) {
            next();
            lt_name = push_token(sema, tok);
            next();
        }
        // Base type name.
        subexpr = parse_type_expr();
        // FIXME: unnatural
        if (subexpr->kind == ExprKind::type) {
            text = name_of_derived_type(sema.name_table,
                                        mut ? TypeKind::var_ref : TypeKind::ref,
                                        subexpr->as<TypeExpr>()->name)
                       ->text;
        }
    } else if (tok.kind == Tok::star) {
        next();
        type_kind = TypeKind::ptr;
        subexpr = parse_type_expr();
        if (subexpr->kind == ExprKind::type) {
            text = name_of_derived_type(sema.name_table, TypeKind::ptr,
                                        subexpr->as<TypeExpr>()->name)
                       ->text;
        }
    } else if (is_ident_or_keyword(tok)) {
        type_kind = TypeKind::value;

        text = std::string{tok.start, static_cast<size_t>(tok.end - tok.start)};
        next();

        subexpr = nullptr;
    } else {
        error_expected("type name");
        return sema.make_node_pos<BadExpr>(pos);
    }

    Name *name = sema.name_table.push(text.c_str());

    return sema.make_node_pos<TypeExpr>(pos, type_kind, name, mut, lt_name,
                                        subexpr);
}

Expr *Parser::parse_unary_expr() {
    auto pos = tok.pos;

    switch (tok.kind) {
    case Tok::number:
    case Tok::string: {
        return parse_literal_expr();
    }
    case Tok::ident: {
        // All UnaryExprKinds with postfix operators should go here.
        // TODO: Do proper op precedence parsing for right-hand-side unary
        // operators, e.g. '.', '()' and '{...}'.
        auto expr = parse_funccall_or_declref_expr();
        expr = parse_member_expr_maybe(expr);
        if (lookahead_structdef()) {
            expr = parse_structdef_maybe(expr);
        }
        return expr;
    }
    case Tok::lbracket: {
        return parse_cast_expr();
    }
    case Tok::star: {
        next();
        auto expr = parse_unary_expr();
        return make_node_range<UnaryExpr>(pos, UnaryExprKind::deref, expr);
    }
    case Tok::kw_var:
    case Tok::ampersand: {
        auto kind = UnaryExprKind::ref;
        if (tok.kind == Tok::kw_var) {
            expect(Tok::kw_var);
            kind = UnaryExprKind::var_ref;
        }
        expect(Tok::ampersand);
        auto expr = parse_unary_expr();
        return make_node_range<UnaryExpr>(pos, kind, expr);
    }
    case Tok::lparen: {
        expect(Tok::lparen);
        auto inside_expr = parse_expr();
        expect(Tok::rparen);
        return make_node_range<UnaryExpr>(pos, UnaryExprKind::paren, inside_expr);
    }
    // TODO: prefix (++), postfix, sign (+/-)
    default: {
        // Because all expressions start with a unary expression, failing here
        // means no other expression could be matched either, so just do a
        // really generic report.
        error_expected("an expression");
        return make_node_range<BadExpr>(pos);
    }
    }
}

namespace {

int binary_op_precedence(const Token &op) {
    switch (op.kind) {
    case Tok::star:
    case Tok::slash:
        return 2;
    case Tok::plus:
    case Tok::minus:
        return 1;
    case Tok::doubleequals:
    case Tok::greaterthan:
    case Tok::lesserthan:
        return 0;
    default:
        // not an operator
        return -1;
    }
}

} // namespace

// Extend a unary expression into binary if possible, by parsing any attached
// RHS. There may be more than one terms in the RHS, all of which is consumed
// by this function. The parsing goes on as long as operators with higher than
// or equal to 'precedence' are seen. Giving it 0 means that this function will
// keep parsing until no more valid binary operators are seen (which has
// negative precedence values).
Expr *Parser::parse_binary_expr_rhs(Expr *lhs, int precedence = 0) {
    Expr *root = lhs;

    while (!is_eos()) {
        int this_prec = binary_op_precedence(tok);

        // If the upcoming op has lower precedence, finish this subexpression.
        // It will be treated as a single term when this function is re-called
        // with lower precedence.
        if (this_prec < precedence)
            return root;

        Token op = tok;
        next();

        // Parse the second term.
        Expr *rhs = parse_unary_expr();

        // We do not know if this term should associate to left or right; e.g.
        // "(a * b) + c" or "a + (b * c)".  We should look ahead for the next
        // operator that follows this term.
        int next_prec = binary_op_precedence(tok);

        // If the next operator has higher precedence ("a + b * c"), parse the
        // RHS as a single subexpression with elevated minimum precedence. Else
        // ("a * b
        // + c"), just treat it as a unary expression.
        if (this_prec < next_prec)
            rhs = parse_binary_expr_rhs(rhs, precedence + 1);

        // Create a new root with the old root as its LHS, and the recursion
        // result as RHS.  This implements left associativity.
        root = sema.make_node<BinaryExpr>(root, op, rhs);
    }

    return root;
}

// If this expression is a member expression with a dot (.) operator, parse as
// such. If not, just pass along the original expression. This function is
// called after the operand expression part is fully parsed.
Expr *Parser::parse_member_expr_maybe(Expr *expr) {
    Expr *result = expr;

    while (tok.kind == Tok::dot) {
        expect(Tok::dot);

        Name *member_name = push_token(sema, tok);
        next();

        result = make_node_range<MemberExpr>(result->pos, result, member_name);
    }

    return result;
}

bool Parser::lookahead_structdef() {
    auto s = save_state();

    if (tok.kind != Tok::lbrace) {
        goto fail;
    }

    // empty ({})
    if (tok.kind == Tok::rbrace) {
        goto success;
    }
    next();

    if (tok.kind != Tok::dot) {
        goto fail;
    }

success:
    restore_state(s);
    return true;

fail:
    restore_state(s);
    return false;
}

// Parse '.memb = expr' part in Struct { .m1 = e1, .m2 = e2, ... }.
std::optional<StructFieldDesignator> Parser::parse_structdef_field() {
    if (!expect(Tok::dot))
        return {};
    Name *name = push_token(sema, tok);
    next();

    if (!expect(Tok::equals))
        return {};

    auto expr = parse_expr();
    if (!expr)
        return {};

    return StructFieldDesignator{name, nullptr, expr};
}

// If this expression has a trailing {...}, parse as a struct definition
// expression.  If not, just pass along the original expression. This function
// is called after the operand expression part is fully parsed.
Expr *Parser::parse_structdef_maybe(Expr *expr) {
    auto pos = tok.pos;

    if (expr->kind != ExprKind::decl_ref) {
        error("qualified struct names are not yet supported");
    }
    auto declrefexpr = static_cast<DeclRefExpr *>(expr);

    expect(Tok::lbrace);

    auto v = parse_comma_separated_list<std::optional<StructFieldDesignator>>(
        [this] { return parse_structdef_field(); });

    std::vector<StructFieldDesignator> desigs;
    for (auto opt_field : v) {
        assert(opt_field.has_value());
        desigs.push_back(*opt_field);
    }

    expect(Tok::rbrace);

    return make_node_range<StructDefExpr>(pos, declrefexpr, desigs);
}

Expr *Parser::parse_expr() {
    auto unary = parse_unary_expr();
    if (!unary) {
        return nullptr;
    }
    auto binary = parse_binary_expr_rhs(unary);
    return parse_member_expr_maybe(binary);
}

void Parser::skip_until(Tok kind) {
    while (!is_eos() && tok.kind != kind)
        next();
}

void Parser::skip_until_any(std::initializer_list<Tok> &kinds) {
    while (!is_eos() && !tok.is_any(kinds))
        next();
}

void Parser::skip_until_end_of_line() {
    while (!is_eos() && tok.kind != Tok::newline)
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

AstNode *Parser::parse_toplevel() {
    switch (tok.kind) {
    case Tok::kw_func:
        return parse_func_decl();
    case Tok::kw_struct:
        return parse_struct_decl();
    case Tok::kw_enum:
        return parse_enum_decl();
    case Tok::kw_extern:
        return parse_extern_decl();
    default:
        error(fmt::format(
            "unexpected '{}' at toplevel",
            std::string{tok.start, static_cast<size_t>(tok.end - tok.start)}));
        skip_to_next_line();
        return nullptr;
    }
}

File *Parser::parse_file() {
    auto file = sema.make_node<File>();

    skip_newlines();

    while (!is_eos()) {
        auto toplevel = parse_toplevel();
        if (!toplevel) {
            continue;
        }
        file->toplevels.push_back(toplevel);
        skip_newlines();
    }

    return file;
}

AstNode *Parser::parse() {
    return parse_file();
}

} // namespace cmp
