open! Core
open! Stdio

let examples =
  [
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb";
    "bvwbjplbgvbhsrlpgdmjqwftvncz";
    "nppdvjthqldpwncqszvftbrmjlhg";
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg";
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw";
  ]

let find_different n a =
  let n_unique a =
    a |> Array.to_list
    |> List.dedup_and_sort ~compare:Char.compare
    |> List.length |> ( = ) n
  in
  let open Option.Let_syntax in
  let%map i, _ =
    Array.findi a ~f:(fun i _ ->
        if i < n - 1 || i = Array.length a - 1 then false
        else n_unique (Array.slice a (i - (n - 1)) (i + 1)))
  in
  i + 1

let part1 input =
  input |> String.to_array |> find_different 4 |> Option.value_exn
  |> Int.to_string

let%expect_test _ =
  let result = List.map ~f:part1 examples in
  print_s [%sexp (result : string list)];
  [%expect {| (7 5 6 10 11) |}]

let part2 input =
  input |> String.to_array |> find_different 14 |> Option.value_exn
  |> Int.to_string

let%expect_test _ =
  let result = List.map ~f:part2 examples in
  print_s [%sexp (result : string list)];
  [%expect {| (19 23 23 29 26) |}]
