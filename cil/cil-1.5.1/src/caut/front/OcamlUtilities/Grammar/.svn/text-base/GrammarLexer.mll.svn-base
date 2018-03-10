{
  open Grammar
  (* Do terminals want to be escaped or not? *)
  let unescape str =
      let stream = Stream.of_string str in
      let buf = Buffer.create (String.length str) in
      let is_escaped = ref false in
      Stream.iter
          (fun ch ->
               if !is_escaped
               then (is_escaped := false;
                     Buffer.add_char
                         buf
                         (match ch with
                              | 'n' -> '\n'
                              | 't' -> '\t'
                              | 'r' -> '\r'
                              | '"' -> '"'
                              | '\\' -> '\\'
                              | _ -> failwith "Illegal backslash sequence"))
               else if ch = '\\' then is_escaped := true
               else Buffer.add_char buf ch)
          stream;
      Buffer.contents buf
}

let escape_sequence = '\\' [ '\\' 'n' 'r' 't' '"' ]
let string_literal = (escape_sequence | [^ '\\' '"'])*
    (* In a string literal, make sure that every quote is preceded by a backslash and that every backslash escapes some special character. *)

let alpha = [ 'a'-'z' 'A'-'Z' ]
rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }
  | "//" [^ '\n']* { token lexbuf } (* Comment until end of line *)
  | "::=" { DEF }
  | '"' (string_literal as str) '"' { TERM { GrammarTypes.text = (*unescape*) str } }
  | alpha (alpha | [ '_' '0'-'9' ])* as str { NONTERM { GrammarTypes.name = str } }
  | '|' { PIPE }
  | ';' { SEMICOLON }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '*' { STAR }
  | '+' { PLUS }
  | eof { EOF }
