open! Base
open! Stdio

type range = { lower : int; upper : int } [@@deriving sexp]

let contains r1 r2 = r1.lower <= r2.lower && r2.upper <= r1.upper
let overlaps r1 r2 = Int.max r1.lower r2.lower <= Int.min r1.upper r2.upper

let example = {|2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8|}

let parse_ranges s =
  let matches =
    let open Re in
    Posix.compile_pat "([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)"
    |> Fn.flip exec s |> Group.all
  in
  match matches with
  | [| _; x1; y1; x2; y2 |] ->
      ( { lower = Int.of_string x1; upper = Int.of_string y1 },
        { lower = Int.of_string x2; upper = Int.of_string y2 } )
  | x ->
      failwith
        ("invalid ranges: " ^ Sexp.to_string_hum ([%sexp_of: string array] x))

let part1 input =
  input |> String.split_lines |> List.map ~f:parse_ranges
  |> List.sum
       (module Int)
       ~f:(fun (r1, r2) -> if contains r1 r2 || contains r2 r1 then 1 else 0)
  |> Int.to_string

let%expect_test _ =
  let result = part1 example in
  print_endline result;
  [%expect {| 2 |}]

let part2 input =
  input |> String.split_lines |> List.map ~f:parse_ranges
  |> List.sum (module Int) ~f:(fun (r1, r2) -> if overlaps r1 r2 then 1 else 0)
  |> Int.to_string

let%expect_test _ =
  let result = part2 example in
  print_endline result;
  [%expect {| 4 |}]
