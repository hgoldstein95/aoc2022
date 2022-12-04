open! Base
open Stdio

let parse_rucksacks s =
  s |> String.to_list |> List.chunks_of ~length:(String.length s / 2)

let find_common = function
  | b :: bs ->
      List.find_exn
        ~f:(fun c ->
          List.for_all ~f:(fun b' -> List.mem ~equal:Char.equal b' c) bs)
        b
  | _ -> failwith "no lists"

let priority c =
  match c with
  | 'a' .. 'z' -> Char.to_int c - 97 + 1
  | 'A' .. 'Z' -> Char.to_int c - 65 + 27
  | _ -> failwith "invalid char"

let%expect_test "priority" =
  print_s [%sexp (priority 'a' : int)];
  [%expect {| 1 |}];
  print_s [%sexp (priority 'z' : int)];
  [%expect {| 26 |}];
  print_s [%sexp (priority 'A' : int)];
  [%expect {| 27 |}];
  print_s [%sexp (priority 'Z' : int)];
  [%expect {| 52 |}]

let part1 input =
  input |> String.split_lines
  |> List.map ~f:parse_rucksacks
  |> List.map ~f:find_common |> List.map ~f:priority
  |> List.sum (module Int) ~f:Fn.id
  |> Int.to_string

let%expect_test "part1" =
  let ex =
    "vJrwpWtwJgWrhcsFMMfFFhFp\n\
     jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
     PmmdzqPrVvPwwTWBwg\n\
     wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
     ttgJtRGJQctTZtZT\n\
     CrZsJsPPZsGzwwsLwLmpwMDw"
  in
  print_endline (part1 ex);
  [%expect {| 157 |}]

let part2 input =
  input |> String.split_lines |> List.map ~f:String.to_list
  |> List.chunks_of ~length:3 |> List.map ~f:find_common |> List.map ~f:priority
  |> List.sum (module Int) ~f:Fn.id
  |> Int.to_string

let%expect_test "part2" =
  let ex =
    "vJrwpWtwJgWrhcsFMMfFFhFp\n\
     jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
     PmmdzqPrVvPwwTWBwg\n\
     wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
     ttgJtRGJQctTZtZT\n\
     CrZsJsPPZsGzwwsLwLmpwMDw"
  in
  print_endline (part2 ex);
  [%expect {| 70 |}]