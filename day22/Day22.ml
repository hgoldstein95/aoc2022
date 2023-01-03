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
  type state = {
    coord : Grid.Point.t;
    facing : Grid.Facing.t;
    cube : Cube.t option;
  }
  [@@deriving sexp]

  let init ?cube (j : Jungle.t) =
    let x =
      j.grid.(0)
      |> Array.findi ~f:(fun _ -> function Jungle.Open -> true | _ -> false)
      |> Option.value_exn |> fst
    in
    { coord = { x; y = 0 }; facing = Grid.Facing.East; cube }

  let step jungle state n =
    let rec loop s last i =
      if i = 0 then s
      else
        let delta = Grid.Facing.delta s.facing in
        let target = Grid.wrap jungle (Grid.Delta.move delta s.coord) in
        match Grid.get_exn jungle target with
        | Jungle.Wall -> { s with coord = last }
        | Jungle.Open -> loop { s with coord = target } target (i - 1)
        | Jungle.Off_map -> loop { s with coord = target } last i
    in
    loop state state.coord n

  let step_cube jungle state n =
    let rec loop s i =
      if i = 0 then s
      else
        let cube = Option.value_exn s.cube in
        let delta = Grid.Facing.delta s.facing in
        let s_target =
          let target = Grid.Delta.move delta s.coord in
          match Grid.get jungle target with
          | None | Some Jungle.Off_map ->
              let turns, new_cube =
                Cube.reorient
                  (match state.facing with
                  | North -> Cube.turn_down cube
                  | South -> Cube.turn_up cube
                  | East -> Cube.turn_right cube
                  | West -> Cube.turn_left cube)
              in
              let new_facing = Grid.Facing.right ~n:turns s.facing in
              let new_coord =
                match (new_facing, state.facing) with _ -> failwith "TODO"
              in
              { coord = new_coord; facing = new_facing; cube = Some new_cube }
          | _ -> state
        in
        match Grid.get_exn jungle s_target.coord with
        | Jungle.Open -> loop s_target (i - 1)
        | Jungle.Wall -> s_target
        | _ -> failwith "Invalid cube state"
    in
    loop state n

  let run (jungle : Jungle.t) (directions : Directions.t) :
      Grid.Point.t * Grid.Facing.t =
    let rec loop state dirs =
      match dirs with
      | [] -> state
      | `Move n :: rest -> (
          match state.cube with
          | None -> loop (step jungle state n) rest
          | Some _ -> loop (step_cube jungle state n) rest)
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
