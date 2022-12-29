type _ t [@@deriving sexp_of]

val arith_args : int t -> int t list
val compile : bool t -> Z3.context -> Z3.Expr.expr
val eval : 'a t -> (string -> int option) -> 'a option
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