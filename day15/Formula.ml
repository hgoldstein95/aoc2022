open! Core

type _ t =
  | Var : string -> int t
  | Int : int -> int t
  | Add : int t * int t -> int t
  | Sub : int t * int t -> int t
  | Abs : int t -> int t
  | Le : int t * int t -> bool t
  | Equal : int t * int t -> bool t
  | True : bool t
  | False : bool t
  | And : bool t list -> bool t
  | Or : bool t list -> bool t
  | Not : bool t -> bool t

let compile (ctx : Z3.context) (expr : bool t) : Z3.Expr.expr =
  let open Z3 in
  let module Integer = Z3.Arithmetic.Integer in
  let rec loop : type a. a t -> Expr.expr = function
    | Var p -> Integer.mk_const_s ctx p
    | True -> Boolean.mk_true ctx
    | False -> Boolean.mk_false ctx
    | Int i -> Integer.mk_numeral_i ctx i
    | Le (x, y) -> Arithmetic.mk_le ctx (loop x) (loop y)
    | Equal (x, y) -> Boolean.mk_eq ctx (loop x) (loop y)
    | And es -> Boolean.mk_and ctx (List.map ~f:loop es)
    | Or es -> Boolean.mk_or ctx (List.map ~f:loop es)
    | Not e -> Boolean.mk_not ctx (loop e)
    | Add (x, y) -> Arithmetic.mk_add ctx [ loop x; loop y ]
    | Sub (x, y) -> Arithmetic.mk_sub ctx [ loop x; loop y ]
    | Abs x ->
        let x = loop x in
        Boolean.mk_ite ctx
          (Arithmetic.mk_ge ctx x (Integer.mk_numeral_i ctx 0))
          x
          (Arithmetic.mk_unary_minus ctx x)
  in
  loop expr

let all es =
  if List.exists es ~f:(function False -> true | _ -> false) then False
  else
    let es = List.filter_map es ~f:(function True -> None | e -> Some e) in
    match es with [] -> True | [ e ] -> e | es -> And es

let any es =
  if List.exists es ~f:(function True -> true | _ -> false) then True
  else
    let es = List.filter_map es ~f:(function False -> None | e -> Some e) in
    match es with [] -> False | [ e ] -> e | es -> Or es

let var s = Var s
let int x = Int x

let ( + ) x y =
  match (x, y) with Int x, Int y -> Int (x + y) | x, y -> Add (x, y)

let ( - ) x y =
  match (x, y) with Int x, Int y -> Int (x - y) | x, y -> Sub (x, y)

let abs = function Int x -> Int (abs x) | x -> Abs x

let ( <= ) x y =
  match (x, y) with
  | Int x, Int y -> if x <= y then True else False
  | x, y -> Le (x, y)

let equal x y =
  match (x, y) with
  | Int x, Int y -> if x = y then True else False
  | x, y -> Equal (x, y)

let ( && ) x y = all [ x; y ]
let ( || ) x y = any [ x; y ]
let not = function True -> False | False -> True | e -> Not e
