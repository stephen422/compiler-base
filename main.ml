open Sedlexing

let digit = [%sedlex.regexp? '0'..'9']

type token =
  | NUMBER
  | IDENTIFIER
  | EOF

let rec lex lexbuf =
  match%sedlex lexbuf with
  | digit ->
    print_endline "NUMBER";
    NUMBER
  | id_start, Star id_continue ->
    print_string "IDENTIFIER: ";
    print_endline (Sedlexing.Utf8.lexeme lexbuf);
    IDENTIFIER
  | Plus white_space ->
    lex lexbuf
  | eof ->
    print_endline "EOF";
    EOF
  | _  ->
    print_string "unknown: ";
    print_endline (Sedlexing.Utf8.lexeme lexbuf);
    EOF

let rec lex_all lexbuf =
  let token = lex lexbuf in
  match token with
  | EOF -> ()
  | _ -> lex_all lexbuf

let () =
  let ch = open_in_bin "main.ml" in
  let lexbuf = Sedlexing.Utf8.from_channel ch in
  lex_all lexbuf
