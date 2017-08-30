open Sedlexing

let digit = [%sedlex.regexp? '0'..'9']

let () =
  let ch = open_in_bin "main.ml" in
  let lexbuf = Sedlexing.Utf8.from_channel ch in
  match%sedlex lexbuf with
  | digit -> print_endline "Hey, int!"
  | id_start, Star id_continue -> print_endline (Sedlexing.Utf8.lexeme lexbuf)
  | _  -> print_endline (Sedlexing.Utf8.lexeme lexbuf)
