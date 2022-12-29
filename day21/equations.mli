exception Not_found

type t [@@deriving sexp_of]

val find : t -> var:string -> int Formula.t option
val find_exn : t -> var:string -> int Formula.t
val get : t -> var:string -> int option
val of_alist : (string * int Formula.t) list -> t
val to_alist : t -> (string * int Formula.t) list
val remove : t -> var:string -> unit
val variables : t -> string list
val to_formula : t -> bool Formula.t
