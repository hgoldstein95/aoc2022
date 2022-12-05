open! Base
open! Stdio

let example =
  "    [D]    \n\
   [N] [C]    \n\
   [Z] [M] [P]\n\
  \ 1   2   3 \n\n\
   move 1 from 2 to 1\n\
   move 3 from 1 to 3\n\
   move 2 from 2 to 1\n\
   move 1 from 1 to 2"

module Command = struct
  type t = { count : int; from : int; to_ : int } [@@deriving sexp]

  let of_string s =
    let matches =
      let open Re in
      Posix.compile_pat "move ([0-9]+) from ([0-9]+) to ([0-9]+)"
      |> Fn.flip exec s |> Group.all
    in
    match matches with
    | [| _; count; from; to_ |] ->
        {
          count = Int.of_string count;
          from = Int.of_string from;
          to_ = Int.of_string to_;
        }
    | x ->
        failwith
          ("invalid command" ^ Sexp.to_string_hum ([%sexp_of: string array] x))
end

module Towers = struct
  type t = char list array [@@deriving sexp]
  (** Each tower list is stored from top to bottom for easy access. *)

  let of_strings s =
    let lines = s |> List.rev |> Fn.flip List.drop 1 in
    let level_of_string l =
      l |> String.to_list |> List.append [ ' ' ] |> List.chunks_of ~length:4
      |> List.map ~f:(fun c ->
             match List.nth_exn c 2 with ' ' -> None | c -> Some c)
    in
    lines
    |> List.map ~f:level_of_string
    |> List.transpose |> Option.value_exn
    |> List.map ~f:List.filter_opt
    |> List.map ~f:List.rev |> Array.of_list

  let execute (towers : t) ~(cmd : Command.t) : t =
    for _ = 0 to cmd.count - 1 do
      match towers.(cmd.from - 1) with
      | [] -> failwith "no disks to move"
      | crate :: from' ->
          towers.(cmd.from - 1) <- from';
          towers.(cmd.to_ - 1) <- crate :: towers.(cmd.to_ - 1)
    done;
    towers

  let execute_9001 (towers : t) ~(cmd : Command.t) : t =
    let from = towers.(cmd.from - 1) in
    if List.length from < cmd.count then failwith "no disks to move"
    else
      let crates, from' = List.split_n from cmd.count in
      towers.(cmd.from - 1) <- from';
      towers.(cmd.to_ - 1) <- List.append crates towers.(cmd.to_ - 1);
      towers
end

let%expect_test _ =
  let result =
    Towers.of_strings
      [ "[D]        "; "[N] [C]    "; "[Z] [M] [P]"; " 1   2   3 " ]
  in
  print_s ([%sexp_of: Towers.t] result);
  [%expect {| ((D N Z) (C M) (P)) |}]

let%expect_test _ =
  let result = Command.of_string "move 5 from 1 to 2" in
  print_s ([%sexp_of: Command.t] result);
  [%expect {| ((count 5) (from 1) (to_ 2)) |}]

let%expect_test _ =
  let result =
    Towers.execute
      ~cmd:{ count = 2; from = 1; to_ = 2 }
      [| [ 'a'; 'b'; 'c' ]; [ 'd'; 'e'; 'f' ] |]
  in
  print_s ([%sexp_of: Towers.t] result);
  [%expect {| ((c) (b a d e f)) |}]

let parse s =
  let lines = String.split_lines s in
  let towers, commands =
    List.split_n lines
      (List.findi_exn lines ~f:(fun _ l -> String.is_empty l) |> fst)
  in
  ( Towers.of_strings towers,
    List.map (List.drop commands 1) ~f:Command.of_string )

let%expect_test "parse" =
  let result = parse example in
  print_s ([%sexp_of: Towers.t * Command.t list] result);
  [%expect
    {|
    (((N Z) (D C M) (P))
     (((count 1) (from 2) (to_ 1)) ((count 3) (from 1) (to_ 3))
      ((count 2) (from 2) (to_ 1)) ((count 1) (from 1) (to_ 2)))) |}]

let part1 input =
  let t, cs = parse input in
  List.fold_left ~init:t ~f:(fun acc c -> Towers.execute ~cmd:c acc) cs
  |> Array.to_list |> List.map ~f:List.hd_exn |> String.of_char_list

let%expect_test _ =
  let result = part1 example in
  print_endline result;
  [%expect {| CMZ |}]

let part2 input =
  let t, cs = parse input in
  List.fold_left ~init:t ~f:(fun acc c -> Towers.execute_9001 ~cmd:c acc) cs
  |> Array.to_list |> List.map ~f:List.hd_exn |> String.of_char_list

let%expect_test _ =
  let result = part2 example in
  print_endline result;
  [%expect {| MCD |}]
