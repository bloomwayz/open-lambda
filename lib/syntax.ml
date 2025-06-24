module Id = struct
  type t = string

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = String.compare
  end)
end

module Expr = struct
  type const = Int of int | Bool of bool
  type id = Id.t
  type bop = Plus | Minus | Times | Eq

  type t =
    | Const of const
    | Var of id
    | Fn of id * t
    | App of t * t
    | Let of id * t * t
    | Rec of id * id * t * t
    | If of t * t * t
    | Bop of bop * t * t
    | Box of t
    | Unbox of t
    | Eval of t

  let indent (lvl : int) (f : 'a -> string) : 'a -> string =
   fun x -> String.init (2 * lvl) (fun _ -> ' ') ^ f x

  let string_of_bop : bop -> string = function
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Eq -> "="

  let rec to_string ?(lvl : int = 0) : t -> string =
    indent lvl @@ function
    | Const (Int n) -> Int.to_string n
    | Const (Bool true) -> "true"
    | Const (Bool false) -> "false"
    | Var x -> x
    | Fn (x, e) ->
        let lvl = lvl + 1 in
        Printf.sprintf "fun %s → (\n%s)" x (to_string ~lvl e)
    | App (e1, e2) ->
        Printf.sprintf "(%s) (%s)" (to_string e1) (to_string ~lvl e2)
    | Let (x, e1, e2) ->
        Printf.sprintf "let %s = %s in\n%s" x (to_string e1) (to_string ~lvl e2)
    | Rec (f, x, e1, e2) ->
        Printf.sprintf "let rec %s %s = %s in\n%s" f x (to_string e1) (to_string ~lvl e2)
    | If (e_pred, e_con, e_alt) ->
        let lvl = lvl + 1 in
        Printf.sprintf "if %s then\n%s else\n%s" (to_string e_pred)
          (to_string ~lvl e_con) (to_string ~lvl e_alt)
    | Bop (op, e1, e2) ->
        Printf.sprintf "(%s %s %s)" (to_string e1) (string_of_bop op)
          (to_string e2)
    | Box e -> Printf.sprintf "`(%s)" (to_string e)
    | Unbox e -> Printf.sprintf ",%s" (to_string e)
    | Eval e -> Printf.sprintf "run %s" (to_string e)
end
