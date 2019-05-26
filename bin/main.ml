open Libthkv

let parse_string filename str =
  let lexbuf = Lexing.from_string str in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try Parser.command Lexer.read lexbuf with
    | Lexer.SyntaxError msg ->
        prerr_endline ("Syntax error: " ^ msg);
        None
    | Parser.Error as x ->
        prerr_endline (Printexc.to_string x);
        None

let rec do_loop db =
  print_string "thkv> ";
  let line = read_line () in
  match parse_string "stdin" line with
    | None -> do_loop db
    | Some(command) -> begin
      let (new_db, result) = Database.exec_command db command in
      print_endline (Data.json_of_value result);
      do_loop new_db
    end

let () = do_loop None
