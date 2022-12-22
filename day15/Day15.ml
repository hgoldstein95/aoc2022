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

  let distance { x = x1; y = y1 } { x = x2; y = y2 } =
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

  let rules_out_beacon (sensor : t) (beacon_candidate : Point.t) : bool =
    let distance_to_closest =
      Point.distance sensor.position sensor.closest_beakon
    in
    let distance_to_candidate =
      Point.distance sensor.position beacon_candidate
    in
    distance_to_candidate <= distance_to_closest
    && not ([%equal: Point.t] beacon_candidate sensor.closest_beakon)

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

  let cannot_contain_beacon (sensors : t) (beacon_candidate : Point.t) : bool =
    List.exists sensors ~f:(fun sensor ->
        Sensor.rules_out_beacon sensor beacon_candidate)

  let could_contain_hidden_beacon (sensors : t) (beacon_candidate : Point.t) :
      bool =
    (not (cannot_contain_beacon sensors beacon_candidate))
    && List.for_all sensors ~f:(fun sensor ->
           not ([%equal: Point.t] sensor.closest_beakon beacon_candidate))

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
         if Sensors.cannot_contain_beacon sensors { x; y } then 1 else 0)
  |> Int.to_string

let%expect_test "part1" =
  let result = part1 ~y:10 example in
  print_endline result;
  [%expect {| 26 |}]

let part2 ?(bound = 4_000_000) input =
  let sensors = Sensors.of_string input in
  List.cartesian_product (List.range 0 bound) (List.range 0 bound)
  |> List.find_exn ~f:(fun (x, y) ->
         Sensors.could_contain_hidden_beacon sensors { x; y })
  |> fun (x, y) -> (x * 4_000_000) + y |> Int.to_string

let%expect_test "part2" =
  let result = part2 ~bound:20 example in
  print_endline result;
  [%expect {| 56000011 |}]
