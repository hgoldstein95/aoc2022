open! Core

type _ t =
  | Var : string -> int t
  | Int : int -> int t
  | Add : int t * int t -> int t
  | Sub : int t * int t -> int t
  | Mul : int t * int t -> int t
  | Div : int t * int t -> int t
  | Abs : int t -> int t
  | Le : int t * int t -> bool t
  | Equal : int t * int t -> bool t
  | True : bool t
  | False : bool t
  | And : bool t list -> bool t
  | Or : bool t list -> bool t
  | Not : bool t -> bool t
  | Ite : bool t * 'a t * 'a t -> 'a t

let arith_args = function
  | Add (e1, e2) -> [ e1; e2 ]
  | Sub (e1, e2) -> [ e1; e2 ]
  | Mul (e1, e2) -> [ e1; e2 ]
  | Div (e1, e2) -> [ e1; e2 ]
  | Abs e -> [ e ]
  | _ -> []

let eval (expr : 'a t) (ctx : string -> int option) : 'a option =
  let open Option.Let_syntax in
  let rec loop : type a. a t -> a option = function
    | Var p -> ctx p
    | True -> Some true
    | False -> Some false
    | Int i -> Some i
    | Le (x, y) ->
        let%bind x = loop x in
        let%map y = loop y in
        x <= y
    | Equal (x, y) ->
        let%bind x = loop x in
        let%map y = loop y in
        x = y
    | And es ->
        let%map es = Option.all (List.map ~f:loop es) in
        List.for_all es ~f:Fn.id
    | Or es ->
        let%map es = Option.all (List.map ~f:loop es) in
        List.exists es ~f:Fn.id
    | Not e ->
        let%map e = loop e in
        not e
    | Add (x, y) ->
        let%bind x = loop x in
        let%map y = loop y in
        x + y
    | Sub (x, y) ->
        let%bind x = loop x in
        let%map y = loop y in
        x - y
    | Mul (x, y) ->
        let%bind x = loop x in
        let%map y = loop y in
        x * y
    | Div (x, y) ->
        let%bind x = loop x in
        let%map y = loop y in
        x / y
    | Abs x ->
        let%map x = loop x in
        abs x
    | Ite (c, t, e) ->
        let%bind c = loop c in
        let%bind t = loop t in
        let%map e = loop e in
        if c then t else e
  in
  loop expr

let compile (expr : bool t) (ctx : Z3.context) : Z3.Expr.expr =
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
    | Mul (x, y) -> Arithmetic.mk_mul ctx [ loop x; loop y ]
    | Div (x, y) -> Arithmetic.mk_div ctx (loop x) (loop y)
    | Abs x ->
        let x = loop x in
        Boolean.mk_ite ctx
          (Arithmetic.mk_ge ctx x (Integer.mk_numeral_i ctx 0))
          x
          (Arithmetic.mk_unary_minus ctx x)
    | Ite (c, t, e) -> Boolean.mk_ite ctx (loop c) (loop t) (loop e)
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

let ( * ) x y =
  match (x, y) with Int x, Int y -> Int (x * y) | x, y -> Mul (x, y)

let ( / ) x y =
  match (x, y) with Int x, Int y -> Int (x / y) | x, y -> Div (x, y)

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

let ite c t e =
  match (c, t, e) with
  | True, t, _ -> t
  | False, _, e -> e
  | c, t, e -> Ite (c, t, e)

let z3_int_from_model ctx model s =
  let open Z3 in
  Model.get_const_interp_e model (Arithmetic.Integer.mk_const_s ctx s)
  |> Option.value_exn |> Expr.to_string |> Int.of_string

let solve_for_variable formula ~var =
  let open Z3 in
  let ctx = mk_context [] in
  let solver = Solver.mk_solver ctx None in
  let _ = Solver.check solver [ compile formula ctx ] in
  let model = Solver.get_model solver |> Option.value_exn in
  z3_int_from_model ctx model var