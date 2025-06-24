{
open Lexing
open Parser

exception SyntaxError of string

let comment_depth = ref 0

let keywords =
  let tbl : (string, token) Hashtbl.t = Hashtbl.create 10 in
  let add_to_tbl (id, tok) = Hashtbl.add tbl id tok in
  List.iter add_to_tbl
    [
      ("true", TRUE);
      ("false", FALSE);
      ("if", IF);
      ("then", THEN);
      ("else", ELSE);
      ("let", LET);
      ("rec", REC);
      ("in", IN);
      ("fun", FUN);
      ("run", RUN)
    ];
  tbl

let to_int s =
  if s.[0] = '~' then -int_of_string (String.sub s 1 (String.length s - 1))
  else int_of_string s

let open_comment depth comment lexbuf =
  incr depth;
  comment lexbuf

let close_comment depth comment lexbuf =
  decr depth;
  if !depth > 0 then comment lexbuf
}

let blank = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

let digit = ['0'-'9']
let int = '~'? digit+

rule read =
  parse
  | blank    { read lexbuf }
  | newline  { new_line lexbuf; read lexbuf }
  | int as n { INT (to_int n) }
  | id as s  { match Hashtbl.find_opt keywords s with Some s -> s | None -> ID s }
  | "(*"     { comment_depth := 1; comment lexbuf; read lexbuf }
  | "->"     { RARROW }
  | '='      { EQ }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { AST }
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | '`'      { QUOTE }
  | ','      { COMMA }
  | eof      { EOF }
  | _        { raise (SyntaxError ("Unexpected char: " ^ lexeme lexbuf)) }

and comment =
  parse
  | "(*"    { open_comment comment_depth comment lexbuf }
  | "*)"    { close_comment comment_depth comment lexbuf }
  | newline { comment lexbuf }
  | eof     { raise (SyntaxError "EOF in comment") }
  | _       { comment lexbuf }