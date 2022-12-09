#!/usr/bin/env fish


if test (count $argv) -lt 1
  echo "Advent of Code 2022 Day Generator"
  echo "Usage: new_day.fish <day>"
  echo "Eg: new_day.fish 12"
  exit 1
else
  set DAY $argv[1]
  set DAY_exe "$DAY"_exe

  if test -d "day$DAY"
    echo "Day $argv[1] already exists"
    exit 1
  end

  echo > "bin/day$DAY_exe.ml" "\
open Day$DAY
open Stdio

let input = In_channel.read_all \"inputs/input$DAY.txt\"

let () =
  print_string \"Part 1: \";
  input |> part1 |> print_endline

let () =
  print_string \"Part 2: \";
  input |> part2 |> print_endline
"

echo >> "bin/dune" "\

(executable
 (name day$DAY_exe)
 (public_name day$DAY_exe)
 (libraries core stdio day$DAY)
 (modules day$DAY_exe)
 (preprocess
  (pps ppx_jane)))
"

  mkdir "day$DAY"
  cd "day$DAY"
  echo > "Day$DAY.mli" "\
val part1 : string -> string
val part2 : string -> string
"

  echo > "Day$DAY.ml" "\
open! Core
open! Stdio

let example = \"\"

let part1 _ = \"\"

let%expect_test \"part1\" =
  let result = part1 example in
  print_endline result;
  [%expect {|  |}]

let part2 _ = \"\"

let%expect_test \"part2\" =
  let result = part2 example in
  print_endline result;
  [%expect {|  |}]
"

  echo > "dune" "\
(library
  (name day$DAY)
  (libraries core stdio)
  (preprocess (pps ppx_jane))
  (inline_tests))
"

  cd -
  dune build
end
exit 0