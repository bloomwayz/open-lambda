open Syntax

exception Run_error of string
exception Type_error of string

module rec Value : sig
  type t = Int of int | Bool of bool | Closure of closure | Staged of t | Unstaged of t
  and closure = fexpr * Env.t
  and fexpr = Fun of Id.t * Expr.t | Rec of Id.t * Id.t * Expr.t

  val to_int : t -> int
  val to_bool : t -> bool
  val to_closure : t -> closure
  val to_string : t -> string
end = struct
  type t = Int of int | Bool of bool | Closure of closure | Staged of t | Unstaged of t
  and closure = fexpr * Env.t
  and fexpr = Fun of Id.t * Expr.t | Rec of Id.t * Id.t * Expr.t

  let to_int = function Int n -> n | _ -> raise (Type_error "not an int")
  let to_bool = function Bool b -> b | _ -> raise (Type_error "not a bool")

  let to_closure = function
    | Closure c -> c
    | _ -> raise (Type_error "not a function")

  let rec to_string = function
    | Int n -> Int.to_string n
    | Bool true -> "true"
    | Bool false -> "false"
    | Closure (Fun (x, e), _) -> "λ" ^ x ^ ".(" ^ Expr.to_string e ^ ")"
    | Closure (Rec (_, x, e), _) -> "λ" ^ x ^ ".(" ^ Expr.to_string e ^ ")"
    | Staged v -> "box " ^ to_string v
    | Unstaged v -> "unbox " ^ to_string v
end

and Env : sig
  type t

  val empty : t
  val add : Id.t -> Value.t -> t -> t
  val find_opt : Id.t -> t -> Value.t option
end = struct
  include Id.Map

  type t = Value.t Id.Map.t
end

let op_to_fn : Expr.bop -> Value.t * Value.t -> Value.t =
  let open Value in
  function
  | Plus -> fun (v1, v2) -> Int (to_int v1 + to_int v2)
  | Minus -> fun (v1, v2) -> Int (to_int v1 - to_int v2)
  | Times -> fun (v1, v2) -> Int (to_int v1 * to_int v2)
  | Eq -> (
      fun (v1, v2) ->
        match (v1, v2) with
        | Int n1, Int n2 -> Bool (n1 = n2)
        | Bool b1, Bool b2 -> Bool Bool.(b1 = b2)
        | _ -> raise (Type_error "Eq operands are not int/bool"))

let rec eval env : Expr.t -> Value.t = function
  | Const (Int n) -> Int n
  | Const (Bool b) -> Bool b
  | Var x -> (
      match Env.find_opt x env with
      | Some v -> v
      | None -> raise (Run_error ("unbound id: " ^ x)))
  | Fn (x, e) -> Closure (Fun (x, e), env)
  | App (e1, e2) -> (
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      let c, env' = Value.to_closure v1 in
      match c with
      | Fun (x, e) -> eval (Env.add x v2 env') e
      | Rec (f, x, e) ->
          let env'' = env' |> Env.add x v2 |> Env.add f v1 in
          eval env'' e)
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      let env' = Env.add x v1 env in
      eval env' e2
  | Rec (f, x, e1, e2) ->
      let closure = Value.Closure (Rec (f, x, e1), env) in
      let env' = Env.add f closure env in
      eval env' e2
  | If (e_pred, e_con, e_alt) ->
      let v1 = eval env e_pred in
      eval env (if Value.to_bool v1 then e_con else e_alt)
  | Bop (op, e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      op_to_fn op (v1, v2)
  | Box e ->
      let v = eval env e in
      Staged v
  | Unbox e -> (
      match (eval env e) with
      | Staged v -> v
      | v -> Unstaged v)
  | Eval e -> raise (Run_error "Eval unimplemented")

let run exp = eval Env.empty exp
