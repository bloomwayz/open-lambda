open Open_lambda

let src = ref ""
let opt_pp = ref false
let opt_run = ref false

let main () =
  Arg.parse
    [
      ("-pp", Arg.Unit (fun _ -> opt_pp := true), "print a program");
      ("-run", Arg.Unit (fun _ -> opt_run := true), "run a program");
    ]
    (fun x -> src := x)
    ("Usage : " ^ Filename.basename Sys.argv.(0) ^ " [-option] [filename] ");
  let lexbuf =
    Lexing.from_channel (if !src = "" then stdin else open_in !src)
  in
  let pgm = Parser.prog Lexer.read lexbuf in
  let open Syntax.Expr in
  if !opt_pp then to_string pgm |> print_endline;
  if not !opt_pp then Interp.run pgm

let () = main ()
