open! Core
open! Stdio

let example =
  "        ...#    \n\
  \        .#..    \n\
  \        #...    \n\
  \        ....    \n\
   ...#.......#    \n\
   ........#...    \n\
   ..#....#....    \n\
   ..........#.    \n\
  \        ...#....\n\
  \        .....#..\n\
  \        .#......\n\
  \        ......#.\n\n\
   10R5L5R10L4R5L5\n"

module Grid = struct
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
end

module Jungle = struct
  type item = Open | Wall | Off_map [@@deriving compare, sexp]
  type t = item Grid.t [@@deriving compare, sexp]

  let of_string (s : string) : t =
    Grid.of_string
      ~parse_char:(function
        | '.' -> Open
        | '#' -> Wall
        | ' ' -> Off_map
        | _ -> failwith "Invalid character in jungle")
      s
end

module Directions = struct
  type t = [ `Move of int | `Turn of Grid.Turn.t ] list
  [@@deriving compare, hash, sexp]

  let of_string (s : string) : t =
    let open Angstrom in
    let move = take_while1 Char.is_digit >>| fun i -> `Move (Int.of_string i) in
    let turn =
      char 'L' *> return (`Turn Grid.Turn.Left)
      <|> char 'R' *> return (`Turn Grid.Turn.Right)
    in
    let directions = many1 (move <|> turn) in
    match parse_string directions s ~consume:All with
    | Ok directions -> directions
    | Error _ -> failwith "Failed to parse directions"

  let%expect_test "of_string" =
    let dirs = of_string "10R5L5R10L4R5L5" in
    print_s [%sexp (dirs : t)];
    [%expect
      {|
      ((Move 10) (Turn Right) (Move 5) (Turn Left) (Move 5) (Turn Right) (Move 10)
       (Turn Left) (Move 4) (Turn Right) (Move 5) (Turn Left) (Move 5)) |}]
end

module Simulation = struct
  type state = { coord : Grid.Point.t; facing : Grid.Facing.t }
  [@@deriving compare, sexp]

  let init (j : Jungle.t) =
    let x =
      j.grid.(0)
      |> Array.findi ~f:(fun _ -> function Jungle.Open -> true | _ -> false)
      |> Option.value_exn |> fst
    in
    { coord = { x; y = 0 }; facing = Grid.Facing.East }

  let run (jungle : Jungle.t) (directions : Directions.t) :
      Grid.Point.t * Grid.Facing.t =
    let rec loop state dirs =
      match dirs with
      | [] -> state
      | `Move n :: rest ->
          let rec step last c i =
            if i = 0 then c
            else
              let delta = Grid.Facing.delta state.facing in
              let target = Grid.wrap jungle (Grid.Delta.move delta c) in
              match Grid.get_exn jungle target with
              | Wall -> last
              | Open -> step target target (i - 1)
              | Off_map -> step last target i
          in
          loop { state with coord = step state.coord state.coord n } rest
      | `Turn turn :: rest ->
          let facing = Grid.Turn.turn turn state.facing in
          loop { state with facing } rest
    in
    let final = loop (init jungle) directions in
    (final.coord, final.facing)
end

let part1 input =
  let lines = input |> String.split_lines in
  let jungle =
    lines |> List.drop_last_exn |> List.drop_last_exn |> String.concat ~sep:"\n"
    |> Jungle.of_string
  in
  let dirs = lines |> List.last_exn |> Directions.of_string in
  let coord, facing = Simulation.run jungle dirs in
  ((1000 * (coord.y + 1))
  + (4 * (coord.x + 1))
  + match facing with East -> 0 | South -> 1 | West -> 2 | North -> 3)
  |> Int.to_string

let%expect_test "part1" =
  let result = part1 example in
  print_endline result;
  [%expect {| 6032 |}]

let part2 _ = ""

let%expect_test "part2" =
  let result = part2 example in
  print_endline result;
  [%expect {|  |}]
