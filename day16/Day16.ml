open! Core
open! Stdio
module G = Quickcheck.Generator
module T = Domainslib.Task

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

  let simulate (network : t) ~(steps : int) ~(parallelism : [ `One | `Two ]) :
      int =
    let module CacheKey = struct
      module T = struct
        type t = (string * (string * int)) list * Set.M(Valve).t
        [@@deriving sexp, equal, compare, hash]
      end

      include T
      include Hashable.Make (T)
    end in
    let pool = T.setup_pool ~num_domains:8 () in
    (* let sort_entities es =
         es |> List.unzip |> fun (ns, rs) ->
         List.zip_exn ns (List.sort rs ~compare:[%compare: string * int])
       in
       let cache = Hashtbl.create (module CacheKey) in *)
    let useful_valves =
      network.valve_map |> Map.data |> List.filter ~f:(fun v -> v.rate > 0)
    in
    let rec loop entities open_valves =
      (* match Hashtbl.find cache (sort_entities entities, open_valves) with
         | Some v -> v
         | None -> *)
      let worlds =
        let open List.Let_syntax in
        let%bind entity_name, (room, steps_left) = entities in
        let%bind new_valve =
          List.filter useful_valves ~f:(fun v -> not (Set.mem open_valves v))
        in
        let distance =
          Hashtbl.find_exn network.distance_map (room, new_valve.name) + 1
        in
        if distance > steps_left then []
        else
          let score = new_valve.rate * (steps_left - distance) in
          let entities' =
            List.Assoc.add ~equal:String.equal entities entity_name
              (new_valve.name, steps_left - distance)
          in
          let open_valves' = Set.add open_valves new_valve in
          List.return
            (T.async pool (fun _ -> score + loop entities' open_valves'))
      in
      let result =
        worlds
        |> List.map ~f:(T.await pool)
        |> List.max_elt ~compare:Int.compare
        |> Option.value ~default:0
      in
      (* Hashtbl.add_exn cache
         ~key:(sort_entities entities, open_valves)
         ~data:result; *)
      result
    in
    let entities =
      match parallelism with
      | `One -> [ ("me", ("AA", steps)) ]
      | `Two -> [ ("me", ("AA", steps)); ("elephant", ("AA", steps)) ]
    in
    let res = T.run pool (fun _ -> loop entities (Set.empty (module Valve))) in
    T.teardown_pool pool;
    res
end

let part1 input =
  input |> Network.of_string
  |> Network.simulate ~steps:30 ~parallelism:`One
  |> Int.to_string

let%expect_test "part1" =
  let result = part1 example in
  print_endline result;
  [%expect {| 1651 |}]

let part2 input =
  input |> Network.of_string
  |> Network.simulate ~steps:26 ~parallelism:`Two
  |> Int.to_string

let%expect_test "part2" =
  let result = part2 example in
  print_endline result;
  [%expect {| 1707 |}]
