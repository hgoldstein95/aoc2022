type _ t

val compile : Z3.context -> bool t -> Z3.Expr.expr
val bvar : string -> bool t
val var : string -> int t
val int : int -> int t
val ( + ) : int t -> int t -> int t
val ( - ) : int t -> int t -> int t
val ( * ) : int t -> int t -> int t
val ( / ) : int t -> int t -> int t
val abs : int t -> int t
val ( <= ) : int t -> int t -> bool t
val equal : int t -> int t -> bool t
val ( && ) : bool t -> bool t -> bool t
val ( || ) : bool t -> bool t -> bool t
val not : bool t -> bool t
val all : bool t list -> bool t
val any : bool t list -> bool t
val ite : bool t -> 'a t -> 'a t -> 'a t
val z3_int_from_model : Z3.context -> Z3.Model.model -> string -> int
val solve_for_variable : bool t -> var:string -> int