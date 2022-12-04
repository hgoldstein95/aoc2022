open! Base
open Stdio
module Q = Base_quickcheck
module G = Q.Generator

module List = struct
  include List
  include Un_tools.List
end

let break_on ~pred xs =
  xs
  |> List.group ~break:(fun a b -> pred a && not (pred b))
  |> List.map ~f:(List.filter ~f:(fun x -> not (pred x)))

let sum = List.fold_left ~f:( + ) ~init:0
let max xs = xs |> List.max_elt ~compare:Int.compare |> Option.value ~default:0

let part1 i =
  i
  |> break_on ~pred:String.is_empty
  |> List.map ~f:(List.map ~f:Int.of_string)
  |> List.map ~f:sum |> max |> Int.to_string

let un_part1 v =
  let open G.Let_syntax in
  let%map xs =
    v |> Int.of_string |> Un_tools.un_max >>= List.map_m ~f:Un_tools.un_sum
  in
  xs
  |> List.map ~f:(List.map ~f:Int.to_string)
  |> List.intersperse ~sep:[ "" ]
  |> List.concat

let%expect_test "part1" =
  Q.Test.run_exn
    (module struct
      type t = string list * string [@@deriving sexp]

      let quickcheck_generator =
        let open G.Let_syntax in
        let%bind s = G.map ~f:Int.to_string (G.int_inclusive 10 100) in
        let%map p = un_part1 s in
        (p, s)

      let quickcheck_shrinker = Q.Shrinker.atomic
    end)
    ~f:(fun (problem, solution) -> [%test_eq: string] (part1 problem) solution)

let part2 i =
  i
  |> break_on ~pred:String.is_empty
  |> List.map ~f:(List.map ~f:Int.of_string)
  |> List.map ~f:sum
  |> List.sort ~compare:(Fn.flip Int.compare)
  |> Fn.flip List.take 3 |> sum |> Int.to_string

let%expect_test "part2" =
  let ex =
    String.split_lines
      "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
  in
  print_endline (part2 ex);
  [%expect {| 45000 |}]
