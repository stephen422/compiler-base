open Sedlexing
open Printf

type token =
  | NUMBER
  | IDENTIFIER
  | EOF

type loc = (int * int)

let digit = [%sedlex.regexp? '0'..'9']

(** Locate the row, col position of the current lexeme. *)
let locate lexbuf =
  Sedlexing.loc lexbuf

(** Lex a token and return its (loc, token, lit). *)
let rec lex lexbuf =
  match%sedlex lexbuf with
  (* skip whitespace *)
  | Plus white_space ->
    lex lexbuf

  | digit ->
    print_endline "NUMBER";
    locate lexbuf, NUMBER

  (* identifiers *)
  | id_start, Star id_continue ->
    let s, e = Sedlexing.loc lexbuf in
    Printf.printf "[%d - %d): IDENTIFIER: " s e;
    print_endline (Sedlexing.Utf8.lexeme lexbuf);
    locate lexbuf, IDENTIFIER

  | eof ->
    print_endline "EOF";
    locate lexbuf, EOF

  | _  ->
    print_string "unknown: ";
    print_endline (Sedlexing.Utf8.lexeme lexbuf);
    (-1, -1), EOF

and lex_all lexbuf =
  let token = lex lexbuf in
  match token with
  | _, EOF -> ()
  | _ -> lex_all lexbuf

let () =
  let ch = open_in_bin Sys.argv.(1) in
  let lexbuf = Sedlexing.Utf8.from_channel ch in
  lex_all lexbuf
