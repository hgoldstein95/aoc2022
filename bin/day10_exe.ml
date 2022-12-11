open Day10
open Stdio

let input = In_channel.read_all "inputs/input10.txt"

let () =
  print_string "Part 1: ";
  input |> part1 |> print_endline

let () =
  print_string "Part 2: ";
  input |> part2 |> print_endline

