open! Core
open! Stdio

let example =
  "[1,1,3,1,1]\n\
   [1,1,5,1,1]\n\n\
   [[1],[2,3,4]]\n\
   [[1],4]\n\n\
   [9]\n\
   [[8,7,6]]\n\n\
   [[4,4],4,4]\n\
   [[4,4],4,4,4]\n\n\
   [7,7,7,7]\n\
   [7,7,7]\n\n\
   []\n\
   [3]\n\n\
   [[[]]]\n\
   [[]]\n\n\
   [1,[2,[3,[4,[5,6,7]]]],8,9]\n\
   [1,[2,[3,[4,[5,6,0]]]],8,9]"

module Packet = struct
  type t = Int of int | List of t list [@@deriving sexp_of, equal]

  let of_string s =
    let open Angstrom in
    let packet =
      fix (fun packet ->
          let int =
            take_while1 (function '0' .. '9' -> true | _ -> false)
            >>| fun x -> Int (Int.of_string x)
          in
          let list =
            char '[' *> sep_by (char ',') packet <* char ']' >>| fun x -> List x
          in
          int <|> list)
    in
    match parse_string ~consume:All packet s with
    | Ok v -> v
    | Error msg -> failwith msg

  let%expect_test "of_string" =
    print_s [%sexp (of_string "[1,2,3]" : t)];
    [%expect {| (List ((Int 1) (Int 2) (Int 3))) |}];
    print_s [%sexp (of_string "[1,[2,3]]" : t)];
    [%expect {| (List ((Int 1) (List ((Int 2) (Int 3))))) |}]

  let rec compare a b =
    match (a, b) with
    | Int a, Int b -> Int.compare a b
    | Int a, List xs -> compare (List [ Int a ]) (List xs)
    | List xs, Int a -> compare (List xs) (List [ Int a ])
    | List [], List [] -> 0
    | List [], List _ -> -1
    | List _, List [] -> 1
    | List (x :: xs), List (y :: ys) -> (
        match compare x y with 0 -> compare (List xs) (List ys) | n -> n)
end

let parse_packets s =
  s |> String.split_lines |> List.chunks_of ~length:3
  |> List.map ~f:(function
       | [ x; y; _ ] -> (Packet.of_string x, Packet.of_string y)
       | [ x; y ] -> (Packet.of_string x, Packet.of_string y)
       | _ -> failwith "invalid input")

let%expect_test "parse_packets" =
  print_s [%sexp (parse_packets example : (Packet.t * Packet.t) list)];
  [%expect
    {|
    (((List ((Int 1) (Int 1) (Int 3) (Int 1) (Int 1)))
      (List ((Int 1) (Int 1) (Int 5) (Int 1) (Int 1))))
     ((List ((List ((Int 1))) (List ((Int 2) (Int 3) (Int 4)))))
      (List ((List ((Int 1))) (Int 4))))
     ((List ((Int 9))) (List ((List ((Int 8) (Int 7) (Int 6))))))
     ((List ((List ((Int 4) (Int 4))) (Int 4) (Int 4)))
      (List ((List ((Int 4) (Int 4))) (Int 4) (Int 4) (Int 4))))
     ((List ((Int 7) (Int 7) (Int 7) (Int 7))) (List ((Int 7) (Int 7) (Int 7))))
     ((List ()) (List ((Int 3))))
     ((List ((List ((List ()))))) (List ((List ()))))
     ((List
       ((Int 1)
        (List
         ((Int 2)
          (List ((Int 3) (List ((Int 4) (List ((Int 5) (Int 6) (Int 7)))))))))
        (Int 8) (Int 9)))
      (List
       ((Int 1)
        (List
         ((Int 2)
          (List ((Int 3) (List ((Int 4) (List ((Int 5) (Int 6) (Int 0)))))))))
        (Int 8) (Int 9))))) |}]

let part1 input =
  input |> parse_packets
  |> List.mapi ~f:(fun i (a, b) -> if Packet.compare a b <= 0 then i + 1 else 0)
  |> List.sum (module Int) ~f:Fn.id
  |> Int.to_string

let%expect_test "part1" =
  let result = part1 example in
  print_endline result;
  [%expect {| 13 |}]

let part2 input =
  let dividers = Packet.[ List [ List [ Int 6 ] ]; List [ List [ Int 2 ] ] ] in
  input |> parse_packets
  |> List.concat_map ~f:(fun (a, b) -> [ a; b ])
  |> List.append dividers
  |> List.sort ~compare:Packet.compare
  |> List.mapi ~f:(fun i x ->
         if List.mem dividers x ~equal:Packet.equal then i + 1 else 1)
  |> List.reduce_exn ~f:( * ) |> Int.to_string

let%expect_test "part2" =
  let result = part2 example in
  print_endline result;
  [%expect {| 140 |}]
