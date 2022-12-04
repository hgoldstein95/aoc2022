open Day1
open Stdio

let () =
  let input = In_channel.read_lines "inputs/input1.txt" in
  print_string "Part 1: ";
  input |> part1 |> print_endline;
  print_string "Part 2: ";
  input |> part2 |> print_endline
