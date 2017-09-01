open Sedlexing
open Printf

type token =
  | NUMBER of string (* lit *)
  | IDENTIFIER of string
  | STRING of string
  | EOL
  | EOF

type loc = (int * int) (* row, col *)

type env =
  { bol_line : int;
    bol_offset : int;
  }

let pretty_print = function
  | NUMBER lit -> "NUMBER: [" ^ lit ^ "]"
  | IDENTIFIER lit -> "IDENTIFIER: [" ^ lit ^ "]"
  | STRING lit -> "STRING: [" ^ lit ^ "]"
  | EOL -> "EOL"
  | _ -> "unknown"

let digit = [%sedlex.regexp? '0'..'9']

(** Locate the row, col position of the current lexeme. *)
let locate env lexbuf =
  let row = env.bol_line in
  let col = Sedlexing.lexeme_start lexbuf - env.bol_offset in
  row, col

(** Lex a token and return its (env, loc, token). *)
let rec lex env lexbuf =
  match%sedlex lexbuf with
  | '\n' ->
    lex_newline env lexbuf

  (* other than \n *)
  | Plus white_space ->
    lex env lexbuf

  | digit ->
    let lit = Sedlexing.Utf8.lexeme lexbuf in
    env, locate env lexbuf, NUMBER lit

  (* identifiers *)
  | id_start, Star id_continue ->
    let lit = Sedlexing.Utf8.lexeme lexbuf in
    env, locate env lexbuf, IDENTIFIER lit

  | '"' ->
    let lit = Buffer.create 16 in
    let token = lex_string lit lexbuf in
    env, locate env lexbuf, token

  | eof ->
    env, locate env lexbuf, EOF

  | _  ->
    print_string "unknown: ";
    print_endline (Sedlexing.Utf8.lexeme lexbuf);
    env, (-1, -1), EOF

and lex_newline env lexbuf =
  let bol_offset = Sedlexing.lexeme_end lexbuf in
  let loc = locate env lexbuf in
  let next_env = {bol_line = env.bol_line + 1;
                  bol_offset; } in
  next_env, loc, EOL

and lex_string lit lexbuf =
  match%sedlex lexbuf with
  | '"' ->
    STRING (Buffer.contents lit)

  | '\\', any ->
    let s = Sedlexing.Utf8.lexeme lexbuf in
    Buffer.add_string lit s;
    lex_string lit lexbuf

  | any ->
    let s = Sedlexing.Utf8.lexeme lexbuf in
    Buffer.add_string lit s;
    lex_string lit lexbuf

  | _ ->
    failwith "unreachable"

and lex_all env lexbuf =
  let env, loc, token = lex env lexbuf in
  match token with
  | EOF -> ()
  | _ ->
    let row, col = loc in
    Printf.printf "%d:%d: " row col;
    print_endline (pretty_print token);
    lex_all env lexbuf

let () =
  let ch = open_in_bin Sys.argv.(1) in
  let lexbuf = Sedlexing.Utf8.from_channel ch in
  lex_all {bol_line = 1; bol_offset = 0;} lexbuf
