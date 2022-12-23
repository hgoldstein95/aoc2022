open! Core
open! Stdio
module G = Quickcheck.Generator

let example =
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\n\
   Valve BB has flow rate=13; tunnels lead to valves CC, AA\n\
   Valve CC has flow rate=2; tunnels lead to valves DD, BB\n\
   Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE\n\
   Valve EE has flow rate=3; tunnels lead to valves FF, DD\n\
   Valve FF has flow rate=0; tunnels lead to valves EE, GG\n\
   Valve GG has flow rate=0; tunnels lead to valves FF, HH\n\
   Valve HH has flow rate=22; tunnel leads to valve GG\n\
   Valve II has flow rate=0; tunnels lead to valves AA, JJ\n\
   Valve JJ has flow rate=21; tunnel leads to valve II"

module Valve = struct
  module T = struct
    let gen_name =
      G.of_list [ "AA"; "BB"; "CC"; "DD"; "EE"; "FF"; "GG"; "HH"; "II"; "JJ" ]

    type t = {
      name : (string[@quickcheck.generator gen_name]);
      rate : (int[@quickcheck.generator Int.gen_incl 0 100]);
      successors :
        (string list[@quickcheck.generator G.list_non_empty gen_name]);
    }
    [@@deriving sexp, equal, compare, quickcheck, hash]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let angstrom =
    let open Angstrom in
    let int = take_while1 Char.is_digit >>| Int.of_string in
    string "Valve " *> take_while1 Char.is_alpha >>= fun name ->
    string " has flow rate=" *> int >>= fun rate ->
    (string "; tunnels lead to valves " <|> string "; tunnel leads to valve ")
    *> sep_by1 (string ", ") (take_while1 Char.is_alpha)
    >>| fun successors -> { name; rate; successors }

  let of_string (s : string) : t =
    let open Angstrom in
    match parse_string ~consume:All angstrom s with
    | Ok t -> t
    | Error _ -> failwith "Failed to parse valve data"

  let to_string (d : t) : string =
    "Valve " ^ d.name ^ " has flow rate=" ^ Int.to_string d.rate
    ^ "; tunnels lead to valves "
    ^ String.concat ~sep:", " d.successors

  let%test_unit "of_string" =
    Quickcheck.test ~sexp_of:[%sexp_of: t] ~trials:1000
      [%quickcheck.generator: t] ~f:(fun d ->
        let s = to_string d in
        [%test_eq: t] d (of_string s))
end

module Network = struct
  module RoomPair = struct
    module T = struct
      type t = string * string [@@deriving sexp, equal, compare, hash]
    end

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)
  end

  type t = {
    valve_map : Valve.t Map.M(String).t;
    distance_map : int Hashtbl.M(RoomPair).t;
  }
  [@@deriving sexp, equal]

  let floyd_warshall (valve_map : Valve.t Map.M(String).t) :
      int Hashtbl.M(RoomPair).t =
    let rooms = Map.keys valve_map in
    let distance_map : int Hashtbl.M(RoomPair).t =
      Hashtbl.create (module RoomPair)
    in
    List.iter (Map.data valve_map) ~f:(fun valve ->
        Hashtbl.set distance_map ~key:(valve.name, valve.name) ~data:0;
        List.iter valve.successors ~f:(fun successor ->
            Hashtbl.set distance_map ~key:(valve.name, successor) ~data:1));
    List.iter rooms ~f:(fun k ->
        List.iter rooms ~f:(fun i ->
            List.iter rooms ~f:(fun j ->
                let cost_ik = Hashtbl.find distance_map (i, k) in
                let cost_kj = Hashtbl.find distance_map (k, j) in
                let cost_ij = Hashtbl.find distance_map (i, j) in
                match (cost_ik, cost_kj, cost_ij) with
                | Some cost_ik, Some cost_kj, Some cost_ij ->
                    if cost_ik + cost_kj < cost_ij then
                      Hashtbl.set distance_map ~key:(i, j)
                        ~data:(cost_ik + cost_kj)
                | Some cost_ik, Some cost_kj, None ->
                    Hashtbl.set distance_map ~key:(i, j)
                      ~data:(cost_ik + cost_kj)
                | None, _, _ -> ()
                | _, None, _ -> ())));
    distance_map

  let of_string (s : string) : t =
    let valve_map =
      s |> String.split_lines
      |> List.map ~f:(fun s ->
             let v = Valve.of_string s in
             (v.name, v))
      |> Map.of_alist_exn (module String)
    in
    let distance_map = floyd_warshall valve_map in
    { valve_map; distance_map }

  let potential_values (network : t) (room : string) (steps_left : int)
      (open_valves : Set.M(Valve).t) : (Valve.t * int * int) list =
    network.valve_map |> Map.data
    |> List.filter_map ~f:(fun other_valve ->
           let dist =
             Hashtbl.find_exn network.distance_map (room, other_valve.name) + 1
           in
           if steps_left < dist then None
           else if Set.mem open_valves other_valve then None
           else if other_valve.rate <= 0 then None
           else Some (other_valve, other_valve.rate * (steps_left - dist), dist))

  let simulate (network : t) : int =
    let module CacheKey = struct
      module T = struct
        type t = string * int * Set.M(Valve).t
        [@@deriving sexp, equal, compare, hash]
      end

      include T
      include Hashable.Make (T)
    end in
    let cache = Hashtbl.create (module CacheKey) in
    let rec loop room steps_left open_valves =
      match Hashtbl.find cache (room, steps_left, open_valves) with
      | Some v -> v
      | None ->
          let result =
            if steps_left <= 0 then 0
            else
              match potential_values network room steps_left open_valves with
              | [] -> 0
              | v ->
                  v
                  |> List.map ~f:(fun (new_valve, value, distance) ->
                         value
                         + loop new_valve.name (steps_left - distance)
                             (Set.add open_valves new_valve))
                  |> List.max_elt ~compare:Int.compare
                  |> Option.value_exn
          in
          let _ =
            Hashtbl.add cache ~key:(room, steps_left, open_valves) ~data:result
          in
          result
    in
    loop "AA" 30 (Set.empty (module Valve))
end

let part1 input =
  input |> Network.of_string |> Network.simulate |> Int.to_string

let%expect_test "part1" =
  let result = part1 example in
  print_endline result;
  [%expect {| 1651 |}]

let part2 _ = ""

let%expect_test "part2" =
  let result = part2 example in
  print_endline result;
  [%expect {|  |}]
