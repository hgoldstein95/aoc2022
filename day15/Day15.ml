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
end

module Formula = struct
  type _ t =
    | Var : string * [ `X | `Y ] -> int t
    | Int : int -> int t
    | Add : int t * int t -> int t
    | Sub : int t * int t -> int t
    | Abs : int t -> int t
    | Le : int t * int t -> bool t
    | Equal : int t * int t -> bool t
    | True : bool t
    | False : bool t
    | And : bool t list -> bool t
    | Or : bool t list -> bool t
    | Not : bool t -> bool t

  let compile (ctx : Z3.context) (expr : bool t) =
    let open Z3 in
    let module Integer = Z3.Arithmetic.Integer in
    let rec loop : type a. a t -> Expr.expr = function
      | Var (p, cmp) ->
          Integer.mk_const_s ctx
            (p ^ "." ^ match cmp with `X -> "x" | `Y -> "y")
      | True -> Boolean.mk_true ctx
      | False -> Boolean.mk_false ctx
      | Int i -> Integer.mk_numeral_i ctx i
      | Le (x, y) -> Arithmetic.mk_le ctx (loop x) (loop y)
      | Equal (x, y) -> Boolean.mk_eq ctx (loop x) (loop y)
      | And es -> Boolean.mk_and ctx (List.map ~f:loop es)
      | Or es -> Boolean.mk_or ctx (List.map ~f:loop es)
      | Not e -> Boolean.mk_not ctx (loop e)
      | Add (x, y) -> Arithmetic.mk_add ctx [ loop x; loop y ]
      | Sub (x, y) -> Arithmetic.mk_sub ctx [ loop x; loop y ]
      | Abs x ->
          let x = loop x in
          Boolean.mk_ite ctx
            (Arithmetic.mk_ge ctx x (Integer.mk_numeral_i ctx 0))
            x
            (Arithmetic.mk_unary_minus ctx x)
    in
    loop expr

  let all es =
    if List.exists es ~f:(function False -> true | _ -> false) then False
    else
      let es = List.filter_map es ~f:(function True -> None | e -> Some e) in
      match es with [] -> True | [ e ] -> e | es -> And es

  let any es =
    if List.exists es ~f:(function True -> true | _ -> false) then True
    else
      let es = List.filter_map es ~f:(function False -> None | e -> Some e) in
      match es with [] -> False | [ e ] -> e | es -> Or es

  let ( && ) x y = And [ x; y ]
  let not = function True -> False | False -> True | e -> Not e

  let ( <= ) x y =
    match (x, y) with
    | Int x, Int y -> if x <= y then True else False
    | x, y -> Le (x, y)

  let ( + ) x y =
    match (x, y) with Int x, Int y -> Int (x + y) | x, y -> Add (x, y)

  let ( - ) x y =
    match (x, y) with Int x, Int y -> Int (x - y) | x, y -> Sub (x, y)

  let abs = function Int x -> Int (abs x) | x -> Abs x
  let var s cmp = Var (s, cmp)
  let int x = Int x

  let equal x y =
    match (x, y) with
    | Int x, Int y -> if x = y then True else False
    | x, y -> Equal (x, y)
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

  let rules_out_beacon (sensor : t) (candidate : string) : bool Formula.t =
    let beacon_range =
      abs (sensor.position.x - sensor.closest_beacon.x)
      + abs (sensor.position.y - sensor.closest_beacon.y)
    in
    let open Formula in
    let candidate_range =
      abs (Var (candidate, `X) - Int sensor.position.x)
      + abs (Var (candidate, `Y) - Int sensor.position.y)
    in
    candidate_range <= Int beacon_range
    && not
         (equal (Var (candidate, `X)) (Int sensor.closest_beacon.x)
         && equal (Var (candidate, `Y)) (Int sensor.closest_beacon.y))

  let span_at_y (sensor : t) (y : int) : (int * int) option =
    let range =
      abs (sensor.position.x - sensor.closest_beacon.x)
      + abs (sensor.position.y - sensor.closest_beacon.y)
    in
    let x_range = range - abs (sensor.position.y - y) in
    if x_range < 0 then None
    else Some (sensor.position.x - x_range, sensor.position.x + x_range)
end

let%expect_test "SensorData.of_string" =
  let results = example |> String.split_lines |> List.map ~f:Sensor.of_string in
  print_s [%sexp (results : Sensor.t list)];
  [%expect
    {|
    (((position ((x 2) (y 18))) (closest_beacon ((x -2) (y 15))))
     ((position ((x 9) (y 16))) (closest_beacon ((x 10) (y 16))))
     ((position ((x 13) (y 2))) (closest_beacon ((x 15) (y 3))))
     ((position ((x 12) (y 14))) (closest_beacon ((x 10) (y 16))))
     ((position ((x 10) (y 20))) (closest_beacon ((x 10) (y 16))))
     ((position ((x 14) (y 17))) (closest_beacon ((x 10) (y 16))))
     ((position ((x 8) (y 7))) (closest_beacon ((x 2) (y 10))))
     ((position ((x 2) (y 0))) (closest_beacon ((x 2) (y 10))))
     ((position ((x 0) (y 11))) (closest_beacon ((x 2) (y 10))))
     ((position ((x 20) (y 14))) (closest_beacon ((x 25) (y 17))))
     ((position ((x 17) (y 20))) (closest_beacon ((x 21) (y 22))))
     ((position ((x 16) (y 7))) (closest_beacon ((x 15) (y 3))))
     ((position ((x 14) (y 3))) (closest_beacon ((x 15) (y 3))))
     ((position ((x 20) (y 1))) (closest_beacon ((x 15) (y 3))))) |}]

module Sensors = struct
  type t = Sensor.t list [@@deriving sexp, equal, compare, quickcheck]

  let of_string s = s |> String.split_lines |> List.map ~f:Sensor.of_string

  let could_contain_hidden_beacon (sensors : t) (candidate : string) :
      bool Formula.t =
    let open Formula in
    (not
       (any
          (List.map sensors ~f:(fun sensor ->
               Sensor.rules_out_beacon sensor candidate))))
    && all
         (List.map sensors ~f:(fun sensor ->
              not
                (equal (Var (candidate, `X)) (Int sensor.closest_beacon.x)
                && equal (Var (candidate, `Y)) (Int sensor.closest_beacon.y))))

  let spans_at_y (sensors : t) ~(y : int) : (int * int) list =
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
    List.filter_map sensors ~f:(fun sensor -> Sensor.span_at_y sensor y)
    |> List.sort ~compare:[%compare: int * int]
    |> collapse_ranges
end

let z3_int_from_model ctx model s =
  let open Z3 in
  Model.get_const_interp_e model (Arithmetic.Integer.mk_const_s ctx s)
  |> Option.value_exn |> Expr.to_string
  |> String.filter ~f:(fun x ->
         not
           (Char.is_whitespace x || [%equal: char] x '(' || [%equal: char] x ')'))
  |> Int.of_string

let part1 ?(y = 2_000_000) input =
  input |> Sensors.of_string |> Sensors.spans_at_y ~y
  |> List.sum (module Int) ~f:(fun (x1, x2) -> x2 - x1)
  |> Int.to_string

let%expect_test "part1" =
  let result = part1 ~y:10 example in
  print_endline result;
  [%expect {| 26 |}]

let part2 ?(bound = 4_000_000) input =
  let sensors = Sensors.of_string input in
  let formula =
    Formula.(
      int 0 <= var "candidate" `X
      && var "candidate" `X <= int bound
      && int 0 <= var "candidate" `Y
      && var "candidate" `Y <= int bound
      && Sensors.could_contain_hidden_beacon sensors "candidate")
  in

  let open Z3 in
  let ctx = mk_context [] in
  let solver = Solver.mk_solver ctx None in
  let _ = Solver.check solver [ Formula.compile ctx formula ] in
  let model = Solver.get_model solver |> Option.value_exn in
  let x = z3_int_from_model ctx model "candidate.x" in
  let y = z3_int_from_model ctx model "candidate.y" in
  (x * 4_000_000) + y |> Int.to_string

let%expect_test "part2" =
  let result = part2 ~bound:20 example in
  print_endline result;
  [%expect {| 56000011 |}]
