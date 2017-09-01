open Sedlexing
open Printf

type token =
  | NUMBER
  | IDENTIFIER
  | STRING of string (* lit *)
  | EOL
  | EOF

type loc = (int * int) (* row, col *)

let digit = [%sedlex.regexp? '0'..'9']

type env =
  { bol_line : int;
    bol_offset : int;
  }

(** Locate the row, col position of the current lexeme. *)
let locate env lexbuf =
  let row = env.bol_line in
  let col = Sedlexing.lexeme_start lexbuf - env.bol_offset in
  Printf.printf "located: %d:%d\n" row col;
  row, col

(** Lex a token and return its (env, loc, token). *)
let rec lex env lexbuf =
  match%sedlex lexbuf with
  | '\n' ->
    print_endline "EOL";
    lex_newline env lexbuf

  (* other than \n *)
  | Plus white_space ->
    lex env lexbuf

  | digit ->
    print_endline "NUMBER";
    env, locate env lexbuf, NUMBER

  (* identifiers *)
  | id_start, Star id_continue ->
    let s, e = Sedlexing.loc lexbuf in
    Printf.printf "[%d - %d): IDENTIFIER: " s e;
    print_endline (Sedlexing.Utf8.lexeme lexbuf);
    env, locate env lexbuf, IDENTIFIER

  | '"' ->
    print_string "STRING: ";
    let lit = Buffer.create 16 in
    let token = lex_string lit lexbuf in
    env, locate env lexbuf, token

  | eof ->
    print_endline "EOF";
    env, locate env lexbuf, EOF

  | _  ->
    print_string "unknown: ";
    print_endline (Sedlexing.Utf8.lexeme lexbuf);
    env, (-1, -1), EOF

and lex_newline env lexbuf =
  let bol_offset = Sedlexing.lexeme_start lexbuf in
  let env = {bol_line = env.bol_line + 1;
             bol_offset; } in
  env, locate env lexbuf, EOL

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
    print_endline "done";
    lex_all env lexbuf

let () =
  let ch = open_in_bin Sys.argv.(1) in
  let lexbuf = Sedlexing.Utf8.from_channel ch in
  lex_all {bol_line = 1; bol_offset = 0;} lexbuf
