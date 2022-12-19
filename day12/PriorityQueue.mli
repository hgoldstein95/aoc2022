open! Core

module type S = sig
  type value
  type priority
  type t

  val empty : t
  val insert : t -> value:value -> priority:priority -> t
  val extract : t -> (priority * value * t) option
end

module M (P : Comparable) (V : Comparable) :
  S with type priority = P.t and type value = V.t
