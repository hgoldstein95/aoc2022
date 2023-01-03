open! Core

module Point = struct
  type t = { x : int; y : int } [@@deriving compare, hash, sexp]
end

module Delta = struct
  type t = { dx : int; dy : int } [@@deriving compare, hash, sexp]

  let move (delta : t) (coord : Point.t) : Point.t =
    Point.{ x = coord.x + delta.dx; y = coord.y + delta.dy }
end

module Facing = struct
  type t = North | South | East | West [@@deriving compare, hash, sexp]

  let delta (facing : t) : Delta.t =
    match facing with
    | North -> { dx = 0; dy = -1 }
    | South -> { dx = 0; dy = 1 }
    | East -> { dx = 1; dy = 0 }
    | West -> { dx = -1; dy = 0 }

  let rec right ?(n = 1) =
    let inner = function
      | North -> East
      | South -> West
      | East -> South
      | West -> North
    in
    Fn.apply_n_times ~n inner

  let left ?(n = 1) =
    let inner = function
      | North -> West
      | South -> East
      | East -> North
      | West -> South
    in
    Fn.apply_n_times ~n inner
end

module Turn = struct
  type t = Left | Right [@@deriving compare, hash, sexp]

  let turn (turn : t) (facing : Facing.t) : Facing.t =
    match (turn, facing) with
    | Left, North -> West
    | Left, South -> East
    | Left, East -> North
    | Left, West -> South
    | Right, North -> East
    | Right, South -> West
    | Right, East -> South
    | Right, West -> North
end

type 'a t = { grid : 'a array array; width : int; height : int }
[@@deriving compare, sexp]

let of_string ~(parse_char : char -> 'a) (s : string) : 'a t =
  let grid =
    let lines = s |> String.split_lines in
    let length =
      List.map lines ~f:String.length
      |> List.max_elt ~compare:Int.compare
      |> Option.value_exn
    in
    lines
    |> List.map ~f:(fun s -> s ^ String.make (length - String.length s) ' ')
    |> List.map ~f:String.to_array
    |> Array.of_list
    |> Array.map ~f:(Array.map ~f:parse_char)
  in
  let width =
    Array.map grid ~f:Array.length
    |> Array.max_elt ~compare:Int.compare
    |> Option.value_exn
  in
  { grid; width; height = Array.length grid }

exception Out_of_bounds

let get (g : 'a t) (coord : Point.t) : 'a option =
  try Some g.grid.(coord.y).(coord.x) with Invalid_argument _ -> None

let get_exn (g : 'a t) (coord : Point.t) : 'a =
  try g.grid.(coord.y).(coord.x)
  with Invalid_argument _ -> raise Out_of_bounds

let wrap (g : 'a t) (coord : Point.t) : Point.t =
  let x = (coord.x + g.width) % g.width in
  let y = (coord.y + g.height) % g.height in
  Point.{ x; y }
