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
    | And : bool t * bool t -> bool t
    | Or : bool t * bool t -> bool t
    | Not : bool t -> bool t

  let eval (expr : bool t) (ctx : (string * Point.t) list) : bool =
    let rec loop : type a. a t -> a = function
      | Var (s, cmp) -> (
          let p = List.Assoc.find_exn ctx s ~equal:String.equal in
          match cmp with `X -> p.x | `Y -> p.y)
      | Int i -> i
      | True -> true
      | False -> false
      | Le (x, y) -> loop x <= loop y
      | Equal (p1, p2) -> loop p1 = loop p2
      | And (e1, e2) -> loop e1 && loop e2
      | Or (e1, e2) -> loop e1 || loop e2
      | Not e -> not (loop e)
      | Add (x, y) -> loop x + loop y
      | Sub (x, y) -> loop x - loop y
      | Abs x -> abs (loop x)
    in
    loop expr

  let optimize (expr : bool t) : bool t =
    let rec loop : type a. a t -> a t = function
      | Var (p, cmp) -> Var (p, cmp)
      | True -> True
      | False -> False
      | Int i -> Int i
      | Le (Int x, Int y) -> if x <= y then True else False
      | Le (x, y) -> Le (loop x, loop y)
      | Equal (Int x, Int y) -> if x = y then True else False
      | Equal (x, y) -> Equal (loop x, loop y)
      | And (True, e) -> loop e
      | And (e, True) -> loop e
      | And (False, _) -> False
      | And (_, False) -> False
      | And (e1, e2) -> And (loop e1, loop e2)
      | Or (True, _) -> True
      | Or (_, True) -> True
      | Or (False, e) -> loop e
      | Or (e, False) -> loop e
      | Or (e1, e2) -> Or (loop e1, loop e2)
      | Not True -> False
      | Not False -> True
      | Not e -> Not (loop e)
      | Add (Int x, Int y) -> Int (x + y)
      | Add (x, y) -> Add (loop x, loop y)
      | Sub (Int x, Int y) -> Int (x - y)
      | Sub (x, y) -> Sub (loop x, loop y)
      | Abs (Int x) -> Int (abs x)
      | Abs x -> Abs (loop x)
    in
    loop expr

  let compile (expr : bool t) : Z3.Expr.expr =
    let open Z3 in
    let module Integer = Z3.Arithmetic.Integer in
    let ctx = mk_context [] in
    let rec loop : type a. a t -> Expr.expr = function
      | Var (p, cmp) ->
          Integer.mk_const_s ctx (p ^ match cmp with `X -> "x" | `Y -> "y")
      | True -> Boolean.mk_true ctx
      | False -> Boolean.mk_false ctx
      | Int i -> Integer.mk_numeral_i ctx i
      | Le (x, y) -> Arithmetic.mk_le ctx (loop x) (loop y)
      | Equal (x, y) -> Boolean.mk_eq ctx (loop x) (loop y)
      | And (e1, e2) -> Boolean.mk_and ctx [ loop e1; loop e2 ]
      | Or (e1, e2) -> Boolean.mk_or ctx [ loop e1; loop e2 ]
      | Not e -> Boolean.mk_not ctx (loop e)
      | Add (x, y) -> Arithmetic.mk_add ctx [ loop x; loop y ]
      | Sub (x, y) -> Arithmetic.mk_sub ctx [ loop x; loop y ]
      | Abs x -> FloatingPoint.mk_abs ctx (loop x)
    in
    loop expr

  let rec all = function e :: es -> And (e, all es) | [] -> True
  let rec any = function e :: es -> Or (e, any es) | [] -> False
  let ( && ) x y = And (x, y)
  let not x = Not x
  let ( <= ) x y = Le (x, y)
  let ( + ) x y = Add (x, y)
  let ( - ) x y = Sub (x, y)
  let abs x = Abs x
  let var s cmp = Var (s, cmp)
  let equal x y = Equal (x, y)
  let int x = Int x

  let point_equal p1 p2 =
    let components p =
      match p with
      | `Var s -> (Var (s, `X), Var (s, `Y))
      | `Point p -> Point.T.(Int p.x, Int p.y)
    in
    let x1, y1 = components p1 in
    let x2, y2 = components p2 in
    Equal (x1, x2) && Equal (y1, y2)

  let distance p1 p2 =
    let components p =
      match p with
      | `Var s -> (Var (s, `X), Var (s, `Y))
      | `Point p -> Point.T.(Int p.x, Int p.y)
    in
    let x1, y1 = components p1 in
    let x2, y2 = components p2 in
    abs (x1 - x2) + abs (y1 - y2)
end

module Sensor = struct
  type t = { position : Point.t; closest_beakon : Point.t }
  [@@deriving sexp, equal, compare, quickcheck]

  let angstrom =
    let open Angstrom in
    string "Sensor at " *> Point.angstrom >>= fun position ->
    string ": closest beacon is at " *> Point.angstrom >>| fun closest_beakon ->
    { position; closest_beakon }

  let of_string s =
    let open Angstrom in
    match parse_string ~consume:All angstrom s with
    | Ok t -> t
    | Error _ -> failwith "Failed to parse sensor data"

  let to_string d =
    "Sensor at " ^ Point.to_string d.position ^ ": closest beacon is at "
    ^ Point.to_string d.closest_beakon

  let%test_unit "of_string" =
    Quickcheck.test ~sexp_of:[%sexp_of: t] ~trials:1000
      [%quickcheck.generator: t] ~f:(fun d ->
        let s = to_string d in
        [%test_eq: t] d (of_string s))

  let rules_out_beacon (sensor : t) (candidate : string) : bool Formula.t =
    let open Formula in
    distance (`Point sensor.position) (`Var candidate)
    <= distance (`Point sensor.position) (`Point sensor.closest_beakon)
    && not (point_equal (`Var candidate) (`Point sensor.closest_beakon))

  let x_boundaries (sensor : t) : int * int =
    let range = Point.distance sensor.position sensor.closest_beakon in
    (sensor.closest_beakon.x - range, sensor.closest_beakon.x + range)
end

let%expect_test "SensorData.of_string" =
  let results = example |> String.split_lines |> List.map ~f:Sensor.of_string in
  print_s [%sexp (results : Sensor.t list)];
  [%expect
    {|
    (((position ((x 2) (y 18))) (closest_beakon ((x -2) (y 15))))
     ((position ((x 9) (y 16))) (closest_beakon ((x 10) (y 16))))
     ((position ((x 13) (y 2))) (closest_beakon ((x 15) (y 3))))
     ((position ((x 12) (y 14))) (closest_beakon ((x 10) (y 16))))
     ((position ((x 10) (y 20))) (closest_beakon ((x 10) (y 16))))
     ((position ((x 14) (y 17))) (closest_beakon ((x 10) (y 16))))
     ((position ((x 8) (y 7))) (closest_beakon ((x 2) (y 10))))
     ((position ((x 2) (y 0))) (closest_beakon ((x 2) (y 10))))
     ((position ((x 0) (y 11))) (closest_beakon ((x 2) (y 10))))
     ((position ((x 20) (y 14))) (closest_beakon ((x 25) (y 17))))
     ((position ((x 17) (y 20))) (closest_beakon ((x 21) (y 22))))
     ((position ((x 16) (y 7))) (closest_beakon ((x 15) (y 3))))
     ((position ((x 14) (y 3))) (closest_beakon ((x 15) (y 3))))
     ((position ((x 20) (y 1))) (closest_beakon ((x 15) (y 3))))) |}]

module Sensors = struct
  type t = Sensor.t list [@@deriving sexp, equal, compare, quickcheck]

  let of_string s = s |> String.split_lines |> List.map ~f:Sensor.of_string

  let cannot_contain_beacon (sensors : t) (candidate : string) : bool Formula.t
      =
    let open Formula in
    any
      (List.map sensors ~f:(fun sensor ->
           Sensor.rules_out_beacon sensor candidate))

  let could_contain_hidden_beacon (sensors : t) (candidate : string) :
      bool Formula.t =
    let open Formula in
    (not (cannot_contain_beacon sensors candidate))
    && all
         (List.map sensors ~f:(fun sensor ->
              not (point_equal (`Var candidate) (`Point sensor.closest_beakon))))

  let x_boundaries (sensors : t) : int * int =
    let x_boundaries = List.map sensors ~f:Sensor.x_boundaries in
    let min_x =
      List.map x_boundaries ~f:fst
      |> List.min_elt ~compare:Int.compare
      |> Option.value_exn
    in
    let max_x =
      List.map x_boundaries ~f:snd
      |> List.max_elt ~compare:Int.compare
      |> Option.value_exn
    in
    (min_x, max_x)
end

let part1 ?(y = 2_000_000) input =
  let sensors = Sensors.of_string input in
  let x_min, x_max = Sensors.x_boundaries sensors in
  List.range x_min x_max
  |> List.sum
       (module Int)
       ~f:(fun x ->
         if
           Formula.eval
             Formula.(
               equal (var "candidate" `Y) (int y)
               && Sensors.cannot_contain_beacon sensors "candidate")
             [ ("candidate", { x; y }) ]
         then 1
         else 0)
  |> Int.to_string

let%expect_test "part1" =
  let result = part1 ~y:10 example in
  print_endline result;
  [%expect {| 26 |}]

let part2 ?(bound = 4_000_000) input =
  let sensors = Sensors.of_string input in
  List.cartesian_product (List.range 0 bound) (List.range 0 bound)
  |> List.find_exn ~f:(fun (x, y) ->
         Formula.eval
           Formula.(
             int 0 <= var "candidate" `X
             && var "candidate" `X <= int bound
             && int 0 <= var "candidate" `Y
             && var "candidate" `Y <= int bound
             && Sensors.could_contain_hidden_beacon sensors "candidate")
           [ ("candidate", { x; y }) ])
  |> fun (x, y) -> (x * 4_000_000) + y |> Int.to_string

let%expect_test "part2" =
  let result = part2 ~bound:20 example in
  print_endline result;
  [%expect {| 56000011 |}]
