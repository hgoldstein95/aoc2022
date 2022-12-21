open Day14
open Stdio

let input = In_channel.read_all "inputs/input14.txt"
let usage_msg = "day14 [-v | --visual]"
let visual = ref false
let speclist = [ ("-visual", Arg.Set visual, "Visualize the solution.") ]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  if not !visual then (
    print_string "Part 1: ";
    input |> part1 |> print_endline;
    print_string "Part 2: ";
    input |> part2 |> print_endline)
  else input |> part2_vis
