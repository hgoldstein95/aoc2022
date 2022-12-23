open! Core
open! Stdio

let example =
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\n\
   Sensor at x=9, y=16: closest beacon is at x=10, y=16\n\
   Sensor at x=13, y=2: closest beacon is at x=15, y=3\n\
   Sensor at x=12, y=14: closest beacon is at x=10, y=16\n\
   Sensor at x=10, y=20: closest beacon is at x=10, y=16\n\
   Sensor at x=14, y=17: closest beacon is at x=10, y=16\n\
   Sensor at x=8, y=7: closest beacon is at x=2, y=10\n\
   Sensor at x=2, y=0: closest beacon is at x=2, y=10\n\
   Sensor at x=0, y=11: closest beacon is at x=2, y=10\n\
   Sensor at x=20, y=14: closest beacon is at x=25, y=17\n\
   Sensor at x=17, y=20: closest beacon is at x=21, y=22\n\
   Sensor at x=16, y=7: closest beacon is at x=15, y=3\n\
   Sensor at x=14, y=3: closest beacon is at x=15, y=3\n\
   Sensor at x=20, y=1: closest beacon is at x=15, y=3"

module Point = struct
  module T = struct
    type t = {
      x : (int[@quickcheck.generator Int.gen_incl (-100) 100]);
      y : (int[@quickcheck.generator Int.gen_incl (-100) 100]);
    }
    [@@deriving sexp, equal, compare, quickcheck]
  end

  include T

  let angstrom =
    let open Angstrom in
    let int =
      option 1 (char '-' >>| fun _ -> -1) >>= fun factor ->
      take_while1 Char.is_digit >>| fun v -> factor * Int.of_string v
    in
    string "x=" *> int >>= fun x ->
    string ", y=" *> int >>| fun y -> { x; y }

  let to_string { x; y } = "x=" ^ Int.to_string x ^ ", y=" ^ Int.to_string y

  let of_string s =
    let open Angstrom in
    match parse_string ~consume:All angstrom s with
    | Ok t -> t
    | Error _ -> failwith "Failed to parse point"

  let%test_unit "of_string" =
    Quickcheck.test ~sexp_of:[%sexp_of: t] ~trials:1000
      [%quickcheck.generator: t] ~f:(fun d ->
        let s = to_string d in
        [%test_eq: t] d (of_string s))

  let distance p1 p2 = abs (p1.x - p2.x) + abs (p1.y - p2.y)
end

module Sensor = struct
  type t = { position : Point.t; closest_beacon : Point.t }
  [@@deriving sexp, equal, compare, quickcheck]

  let angstrom =
    let open Angstrom in
    string "Sensor at " *> Point.angstrom >>= fun position ->
    string ": closest beacon is at " *> Point.angstrom >>| fun closest_beacon ->
    { position; closest_beacon }

  let of_string s =
    let open Angstrom in
    match parse_string ~consume:All angstrom s with
    | Ok t -> t
    | Error _ -> failwith "Failed to parse sensor data"

  let to_string d =
    "Sensor at " ^ Point.to_string d.position ^ ": closest beacon is at "
    ^ Point.to_string d.closest_beacon

  let%test_unit "of_string" =
    Quickcheck.test ~sexp_of:[%sexp_of: t] ~trials:1000
      [%quickcheck.generator: t] ~f:(fun d ->
        let s = to_string d in
        [%test_eq: t] d (of_string s))

  let many_of_string s = s |> String.split_lines |> List.map ~f:of_string

  let could_contain_hidden_beacon (sensors : t list) (cx : int Formula.t)
      (cy : int Formula.t) : bool Formula.t =
    let open Formula in
    let rules_out_candidate sensor =
      let candidate_range =
        abs (cx - Int sensor.position.x) + abs (cy - Int sensor.position.y)
      in
      let beacon_range =
        Int (Point.distance sensor.position sensor.closest_beacon)
      in
      candidate_range <= beacon_range
    in
    let not_candidate (p : Point.t) =
      not (equal cx (Int p.x) && equal cy (Int p.y))
    in
    (not (any (List.map sensors ~f:rules_out_candidate)))
    && all
         (List.map sensors ~f:(fun sensor ->
              not_candidate sensor.closest_beacon))

  let spans_at_y (sensors : t list) ~(y : int) : (int * int) list =
    let span sensor =
      let range = Point.distance sensor.position sensor.closest_beacon in
      let x_range = range - abs (sensor.position.y - y) in
      if x_range < 0 then None
      else Some (sensor.position.x - x_range, sensor.position.x + x_range)
    in
    let ranges_overlap (x_lo, x_hi) (y_lo, y_hi) =
      (x_lo <= y_lo && y_lo <= x_hi) || (y_lo <= x_lo && x_lo <= y_hi)
    in
    let merge_ranges (x_lo, x_hi) (y_lo, y_hi) =
      (min x_lo y_lo, max x_hi y_hi)
    in
    let rec collapse_ranges = function
      | [] -> []
      | [ x ] -> [ x ]
      | x :: y :: xs ->
          if ranges_overlap x y then collapse_ranges (merge_ranges x y :: xs)
          else x :: collapse_ranges (y :: xs)
    in
    List.filter_map sensors ~f:span
    |> List.sort ~compare:[%compare: int * int]
    |> collapse_ranges
end

let part1 ?(y = 2_000_000) input =
  input |> Sensor.many_of_string |> Sensor.spans_at_y ~y
  |> List.sum (module Int) ~f:(fun (x1, x2) -> x2 - x1)
  |> Int.to_string

let%expect_test "part1" =
  let result = part1 ~y:10 example in
  print_endline result;
  [%expect {| 26 |}]

let part2 ?(bound = 4_000_000) input =
  let cx = "candidate.x" in
  let cy = "candidate.y" in
  let sensors = Sensor.many_of_string input in
  let formula =
    let open Formula in
    int 0 <= var cx
    && var cx <= int bound
    && int 0 <= var cy
    && var cy <= int bound
    && Sensor.could_contain_hidden_beacon sensors (var cx) (var cy)
  in

  let open Z3 in
  let z3_int_from_model ctx model s =
    Model.get_const_interp_e model (Arithmetic.Integer.mk_const_s ctx s)
    |> Option.value_exn |> Expr.to_string
    |> String.filter ~f:(fun x ->
           not
             (Char.is_whitespace x
             || [%equal: char] x '('
             || [%equal: char] x ')'))
    |> Int.of_string
  in
  let ctx = mk_context [] in
  let solver = Solver.mk_solver ctx None in
  let _ = Solver.check solver [ Formula.compile ctx formula ] in
  let model = Solver.get_model solver |> Option.value_exn in
  let x = z3_int_from_model ctx model cx in
  let y = z3_int_from_model ctx model cy in
  (x * 4_000_000) + y |> Int.to_string

let%expect_test "part2" =
  let result = part2 ~bound:20 example in
  print_endline result;
  [%expect {| 56000011 |}]
