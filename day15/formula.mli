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

val compile : Z3.context -> bool t -> Z3.Expr.expr
val all : bool t list -> bool t
val any : bool t list -> bool t
val ( && ) : bool t -> bool t -> bool t
val not : bool t -> bool t
val ( <= ) : int t -> int t -> bool t
val ( + ) : int t -> int t -> int t
val ( - ) : int t -> int t -> int t
val abs : int t -> int t
val var : string -> int t
val int : int -> int t
val equal : int t -> int t -> bool t
