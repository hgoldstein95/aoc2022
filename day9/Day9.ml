open! Core
open! Stdio

let example = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"
let example2 = "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"

type dir = Left | Right | Up | Down [@@deriving sexp_of, equal]

module Position = struct
  type t = { x : int; y : int } [@@deriving sexp_of, equal, hash, compare]

  let move ({ x; y } : t) = function
    | Left -> { x = x - 1; y }
    | Right -> { x = x + 1; y }
    | Up -> { x; y = y + 1 }
    | Down -> { x; y = y - 1 }

  let distance from to_ = max (abs (from.x - to_.x)) (abs (from.y - to_.y))

  let direction from to_ =
    (if from.x < to_.x then [ Right ]
    else if from.x > to_.x then [ Left ]
    else [])
    @ if from.y < to_.y then [ Up ] else if from.y > to_.y then [ Down ] else []
end

module Position_ordered = struct
  include Position
  include Comparator.Make (Position)
end

module Command = struct
  type t = dir * int [@@deriving sexp_of, equal]

  let _of_string s =
    let dir_of_string = function
      | "L" -> Left
      | "R" -> Right
      | "U" -> Up
      | "D" -> Down
      | _ -> failwith "invalid direction"
    in
    match String.split_on_chars ~on:[ ' ' ] s with
    | [ dir; len ] -> (dir_of_string dir, Int.of_string len)
    | _ -> failwith "invalid command"
end

module Simulation = struct
  type t = {
    head : Position.t;
    tail : Position.t list;
    tail_visited : Set.M(Position_ordered).t;
  }
  [@@deriving sexp_of]

  let step_toward from to_ =
    if Position.distance from to_ <= 1 then from
    else
      Position.direction from to_
      |> List.fold_left ~init:from ~f:(fun t d -> Position.move t d)

  let step_once ({ head; tail; tail_visited } : t) dir =
    let head' = Position.move head dir in
    let rec loop ahead = function
      | [] -> []
      | t :: ts ->
          let t' = step_toward t ahead in
          t' :: loop t' ts
    in
    let tail' = loop head' tail in
    {
      head = head';
      tail = tail';
      tail_visited = Set.add tail_visited (List.last_exn tail');
    }

  let%expect_test "step_once" =
    let s =
      {
        head = { x = 0; y = 0 };
        tail = [ { x = 0; y = 0 } ];
        tail_visited = Set.singleton (module Position_ordered) { x = 0; y = 0 };
      }
    in
    let s = step_once s Right in
    print_s [%sexp (s : t)];
    [%expect
      {| ((head ((x 1) (y 0))) (tail (((x 0) (y 0)))) (tail_visited (((x 0) (y 0))))) |}];
    let s = step_once s Right in
    print_s [%sexp (s : t)];
    [%expect
      {|
      ((head ((x 2) (y 0))) (tail (((x 1) (y 0))))
       (tail_visited (((x 0) (y 0)) ((x 1) (y 0))))) |}];
    let s = step_once s Up in
    print_s [%sexp (s : t)];
    [%expect
      {|
      ((head ((x 2) (y 1))) (tail (((x 1) (y 0))))
       (tail_visited (((x 0) (y 0)) ((x 1) (y 0))))) |}];
    let s = step_once s Up in
    print_s [%sexp (s : t)];
    [%expect
      {|
      ((head ((x 2) (y 2))) (tail (((x 2) (y 1))))
       (tail_visited (((x 0) (y 0)) ((x 1) (y 0)) ((x 2) (y 1))))) |}]

  let step s (dir, len) =
    List.init len ~f:(fun _ -> dir) |> List.fold_left ~init:s ~f:step_once

  let simulate_rope knots cmds =
    let s0 =
      {
        head = { x = 0; y = 0 };
        tail = List.init (knots - 1) ~f:(fun _ : Position.t -> { x = 0; y = 0 });
        tail_visited = Set.singleton (module Position_ordered) { x = 0; y = 0 };
      }
    in
    List.fold_left ~init:s0 ~f:step cmds
end

let part1 input =
  let cmds = input |> String.split_lines |> List.map ~f:Command._of_string in
  (Simulation.simulate_rope 2 cmds).tail_visited |> Set.length |> Int.to_string

let%expect_test "part1" =
  let result = part1 example in
  print_endline result;
  [%expect {| 13 |}]

let part2 input =
  let cmds = input |> String.split_lines |> List.map ~f:Command._of_string in
  (Simulation.simulate_rope 10 cmds).tail_visited |> Set.length |> Int.to_string

let%expect_test "part2" =
  let result = part2 example2 in
  print_endline result;
  [%expect {| 36 |}]
