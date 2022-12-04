module G = Base_quickcheck.Generator

val un_max : int -> int list G.t
val un_sum : int -> int list G.t

module List : sig
  val sequence : 'a G.t list -> 'a list G.t
  val map_m : 'a list -> f:('a -> 'b G.t) -> 'b list G.t
end
