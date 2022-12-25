open! Core
open! Stdio

let example = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

module Point = struct
  type delta = { dx : int; dy : int } [@@deriving sexp, equal, compare, hash]

  module T = struct
    type t = { x : int; y : int } [@@deriving sexp, equal, compare, hash]
  end

  include T
  include Comparable.Make (T)

  let displace { x; y } { dx; dy } = { x = x + dx; y = y + dy }
end

module Rock = struct
  type shape = { deltas : Point.delta list; height : int; width : int }
  [@@deriving sexp, equal, compare]

  type t = { position : Point.t; shape : shape }
  [@@deriving sexp, equal, compare]

  let points r = List.map r.shape.deltas ~f:(Point.displace r.position)
  let x_max r = r.position.x + r.shape.width - 1
  let y_max r = r.position.y + r.shape.height - 1
  let x_min r = r.position.x
  let y_min r = r.position.y

  let collide r1 r2 =
    ((y_min r1 <= y_min r2 && y_min r2 <= y_max r1)
    || (y_min r2 <= y_min r1 && y_min r1 <= y_max r2))
    && List.exists (points r1) ~f:(fun p ->
           List.mem (points r2) p ~equal:Point.equal)

  let move r = function
    | `Left ->
        { r with position = Point.displace r.position { dx = -1; dy = 0 } }
    | `Right ->
        { r with position = Point.displace r.position { dx = 1; dy = 0 } }
    | `Down ->
        { r with position = Point.displace r.position { dx = 0; dy = -1 } }

  let shapes =
    let open Point in
    [|
      (* horiz *)
      {
        deltas =
          [
            { dx = 0; dy = 0 };
            { dx = 1; dy = 0 };
            { dx = 2; dy = 0 };
            { dx = 3; dy = 0 };
          ];
        height = 1;
        width = 4;
      };
      (* plus *)
      {
        deltas =
          [
            { dx = 1; dy = 0 };
            { dx = 0; dy = 1 };
            { dx = 1; dy = 1 };
            { dx = 2; dy = 1 };
            { dx = 1; dy = 2 };
          ];
        height = 3;
        width = 3;
      };
      (* el *)
      {
        deltas =
          [
            { dx = 0; dy = 0 };
            { dx = 1; dy = 0 };
            { dx = 2; dy = 0 };
            { dx = 2; dy = 1 };
            { dx = 2; dy = 2 };
          ];
        height = 3;
        width = 3;
      };
      (* vert *)
      {
        deltas =
          [
            { dx = 0; dy = 0 };
            { dx = 0; dy = 1 };
            { dx = 0; dy = 2 };
            { dx = 0; dy = 3 };
          ];
        height = 4;
        width = 1;
      };
      (* square *)
      {
        deltas =
          [
            { dx = 0; dy = 0 };
            { dx = 0; dy = 1 };
            { dx = 1; dy = 0 };
            { dx = 1; dy = 1 };
          ];
        height = 2;
        width = 2;
      };
    |]
end

module Instructions = struct
  type t = [ `Left | `Right ] array [@@deriving sexp, equal, compare]

  let of_string (s : string) : t =
    s
    |> String.filter ~f:(fun c -> not (Char.equal '\n' c))
    |> String.to_array
    |> Array.map ~f:(function
         | '<' -> `Left
         | '>' -> `Right
         | _ -> failwith "invalid instruction")
end

module Tetris = struct
  type state = {
    instructions : Instructions.t;
    mutable next_instruction : int;
    mutable next_rock : int;
    mutable rocks : Rock.t list;
    mutable height : int;
  }

  let display ?y_min ?y_max state =
    let y_min = max 0 (Option.value y_min ~default:0) in
    let y_max =
      Option.value y_max
        ~default:
          (3
          + (state.rocks |> List.map ~f:Rock.y_max
            |> List.max_elt ~compare:Int.compare
            |> Option.value ~default:0))
    in
    (if y_min = 0 then "+-------+" else "|       |")
    :: (List.range y_min y_max ~start:`inclusive ~stop:`inclusive
       |> List.map ~f:(fun y ->
              let inner =
                List.range 0 7 ~start:`inclusive
                |> List.map ~f:(fun x ->
                       if
                         List.exists state.rocks ~f:(fun rock ->
                             List.mem (Rock.points rock) { x; y }
                               ~equal:Point.equal)
                       then '#'
                       else '.')
              in
              ('|' :: inner) @ [ '|' ] |> String.of_char_list))
    |> List.rev |> String.concat ~sep:"\n"

  let create instructions =
    {
      instructions;
      next_instruction = 0;
      next_rock = 0;
      rocks = [];
      height = 0;
    }

  let next_instruction state =
    let instruction = state.instructions.(state.next_instruction) in
    state.next_instruction <-
      (state.next_instruction + 1) % Array.length state.instructions;
    instruction

  let next_rock state =
    let shape = Rock.shapes.(state.next_rock) in
    state.next_rock <- (state.next_rock + 1) % Array.length Rock.shapes;
    let position = Point.{ x = 2; y = state.height + 3 } in
    Rock.{ position; shape }

  let move_rock state rock dir =
    let new_rock = Rock.move rock dir in
    if
      Rock.x_min new_rock < 0
      || Rock.x_max new_rock > 6
      || Rock.y_min new_rock < 0
      || List.exists state.rocks ~f:(Rock.collide new_rock)
    then None
    else Some new_rock

  let last_rock_made_tetris state =
    let last_rock = List.hd_exn state.rocks in
    let ys =
      List.range (Rock.y_min last_rock) (Rock.y_max last_rock) ~start:`inclusive
        ~stop:`inclusive
    in
    List.exists ys ~f:(fun y ->
        List.for_all (List.range 0 6 ~start:`inclusive ~stop:`inclusive)
          ~f:(fun x ->
            List.exists state.rocks ~f:(fun rock ->
                List.mem (Rock.points rock) { x; y } ~equal:Point.equal)))

  let drop_rock state =
    let rock = ref (next_rock state) in
    let stopped = ref false in
    while not !stopped do
      rock :=
        move_rock state !rock (next_instruction state)
        |> Option.value ~default:!rock;
      match move_rock state !rock `Down with
      | Some new_rock -> rock := new_rock
      | None ->
          stopped := true;
          state.rocks <- !rock :: state.rocks;
          state.height <- max state.height (Rock.y_max !rock + 1)
    done
end

let part1 input =
  let tetris = input |> Instructions.of_string |> Tetris.create in
  for _ = 1 to 2022 do
    Tetris.drop_rock tetris
  done;
  tetris.height |> Int.to_string

let%expect_test "part1" =
  let result = part1 example in
  print_endline result;
  [%expect {| 3068 |}]

let part2 input =
  let module SavedState = struct
    module T = struct
      type t = int * int * string [@@deriving sexp, compare, hash]
    end

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)
  end in
  let instructions = Instructions.of_string input in
  let total_steps = 1_000_000_000_000 in
  let tetris = Tetris.create instructions in
  let gap = 20 in
  let seen = Hashtbl.create (module SavedState) in
  let found_cycle = ref None in
  let i = ref 0 in
  while Option.is_none !found_cycle do
    Tetris.drop_rock tetris;
    let key =
      ( tetris.next_instruction,
        tetris.next_rock,
        Tetris.display tetris ~y_min:(tetris.height - gap) )
    in
    match Hashtbl.find seen key with
    | None ->
        Hashtbl.add_exn seen ~key ~data:!i;
        i := !i + 1
    | Some prev -> found_cycle := Some (!i - prev)
  done;
  let cycle_start = !i - Option.value_exn !found_cycle in
  let cycle_length = Option.value_exn !found_cycle in
  let start_height = tetris.height in
  for _ = 1 to cycle_length do
    Tetris.drop_rock tetris
  done;
  let end_height = tetris.height in
  let cycle_height = end_height - start_height in
  let remainder = (total_steps - cycle_start) % cycle_length in
  for _ = 1 to remainder do
    Tetris.drop_rock tetris
  done;
  let remainder_height = tetris.height - end_height - 1 in
  start_height
  + ((((total_steps - cycle_start) / cycle_length) - 1) * cycle_height)
  + remainder_height
  |> Int.to_string

let%expect_test "part2" =
  let result = part2 example in
  print_endline result;
  [%expect {| 1514285714288 |}]
