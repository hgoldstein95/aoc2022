open! Base
open Stdio

type choice = Rock | Paper | Scissors [@@deriving equal]
type outcome = Win | Lose | Draw [@@deriving equal]
type game = { opponent : choice; me : choice; outcome : outcome }

let game_table =
  [
    { opponent = Rock; me = Rock; outcome = Draw };
    { opponent = Rock; me = Paper; outcome = Win };
    { opponent = Rock; me = Scissors; outcome = Lose };
    { opponent = Paper; me = Rock; outcome = Lose };
    { opponent = Paper; me = Paper; outcome = Draw };
    { opponent = Paper; me = Scissors; outcome = Win };
    { opponent = Scissors; me = Rock; outcome = Win };
    { opponent = Scissors; me = Paper; outcome = Lose };
    { opponent = Scissors; me = Scissors; outcome = Draw };
  ]

type versus = { opponent : choice; me : choice }

let parse_choice = function
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors
  | _ -> failwith "Invalid choice"

let parse_versus l =
  match String.split ~on:' ' l with
  | [ a; b ] -> { opponent = parse_choice a; me = parse_choice b }
  | _ -> failwith "Invalid input"

let outcome_value = function Win -> 6 | Draw -> 3 | Lose -> 0
let choice_value = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let versus_outcome v =
  (List.find_exn
     ~f:(fun g ->
       [%equal: choice] g.opponent v.opponent && [%equal: choice] g.me v.me)
     game_table)
    .outcome

let score game = choice_value game.me + outcome_value (versus_outcome game)

let part1 input =
  input |> String.split_lines |> List.map ~f:parse_versus |> List.map ~f:score
  |> List.fold_left ~f:( + ) ~init:0
  |> Int.to_string

let%expect_test "part1" =
  let ex = "A Y\nB X\nC Z" in
  print_endline (part1 ex);
  [%expect {| 15 |}]

type rigged = { opponent : choice; outcome : outcome }

let parse_outcome = function
  | "X" -> Lose
  | "Y" -> Draw
  | "Z" -> Win
  | _ -> failwith "Invalid choice"

let parse_rigged l =
  match String.split ~on:' ' l with
  | [ a; b ] -> { opponent = parse_choice a; outcome = parse_outcome b }
  | _ -> failwith "Invalid input"

let rigged_choice v =
  (List.find_exn
     ~f:(fun g ->
       [%equal: choice] g.opponent v.opponent
       && [%equal: outcome] g.outcome v.outcome)
     game_table)
    .me

let part2 input =
  input |> String.split_lines |> List.map ~f:parse_rigged
  |> List.map ~f:(fun r ->
         score { opponent = r.opponent; me = rigged_choice r })
  |> List.fold_left ~f:( + ) ~init:0
  |> Int.to_string

let%expect_test "part2" =
  let ex = "A Y\nB X\nC Z" in
  print_endline (part2 ex);
  [%expect {| 12 |}]
