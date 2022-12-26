open! Core
open! Stdio

let example =
  "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each \
   obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 \
   obsidian.\n\
   Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each \
   obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 \
   obsidian."

module Resource = struct
  module T = struct
    type t = Ore | Clay | Obsidian | Geode
    [@@deriving sexp, equal, compare, hash]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module ResourceSet = struct
  module T = struct
    type t = int Map.M(Resource).t [@@deriving sexp, equal, compare, hash]
  end

  include T

  let empty = Map.empty (module Resource)
  let uniform r n = Map.of_alist_exn (module Resource) [ (r, n) ]

  let add (a : t) (b : t) : t =
    Map.merge a b ~f:(fun ~key:_ -> function
      | `Both (a, b) -> Some (a + b) | `Left a | `Right a -> Some a)

  let sub (a : t) (b : t) : t option =
    List.concat_map
      Resource.[ Ore; Clay; Obsidian; Geode ]
      ~f:(fun resource ->
        match (Map.find a resource, Map.find b resource) with
        | Some stock, Some cost ->
            [ (if cost <= stock then Some (resource, stock - cost) else None) ]
        | Some stock, None -> [ Some (resource, stock) ]
        | None, Some _ -> [ None ]
        | None, None -> [])
    |> Option.all
    |> Option.map ~f:(Map.of_alist_exn (module Resource))

  include Comparable.Make (T)
  include Hashable.Make (T)
end

module Blueprint = struct
  module T = struct
    type t = { id : int; costs : ResourceSet.t Map.M(Resource).t }
    [@@deriving sexp, equal, compare, hash]
  end

  include T

  let angstrom =
    let open Angstrom in
    let int = take_while1 Char.is_digit >>| Int.of_string in
    let item =
      int <* string " ore" >>| ResourceSet.uniform Ore
      <|> (int <* string " clay" >>| ResourceSet.uniform Clay)
      <|> (int <* string " obsidian" >>| ResourceSet.uniform Obsidian)
      <|> (int <* string " geode" >>| ResourceSet.uniform Geode)
    in
    let price name =
      string ("Each " ^ name ^ " robot costs ") *> sep_by1 (string " and ") item
      <* char '.'
      >>| fun items ->
      List.fold items ~init:ResourceSet.empty ~f:ResourceSet.add
    in
    string "Blueprint " *> int >>= fun id ->
    string ": " *> price "ore" >>= fun ore ->
    string " " *> price "clay" >>= fun clay ->
    string " " *> price "obsidian" >>= fun obsidian ->
    string " " *> price "geode" >>| fun geode ->
    {
      id;
      costs =
        Map.of_alist_exn
          (module Resource)
          [ (Ore, ore); (Clay, clay); (Obsidian, obsidian); (Geode, geode) ];
    }

  let of_string s =
    match Angstrom.parse_string ~consume:All angstrom s with
    | Ok blueprint -> blueprint
    | Error msg -> failwith msg

  include Comparable.Make (T)
  include Hashable.Make (T)
end

let%expect_test "Blueprint.to_string" =
  let blueprints =
    example |> String.split_lines |> List.map ~f:Blueprint.of_string
  in
  print_s [%sexp (blueprints : Blueprint.t list)];
  [%expect
    {|
    (((id 1)
      (costs
       ((Ore ((Ore 4))) (Clay ((Ore 2))) (Obsidian ((Ore 3) (Clay 14)))
        (Geode ((Ore 2) (Obsidian 7))))))
     ((id 2)
      (costs
       ((Ore ((Ore 2))) (Clay ((Ore 3))) (Obsidian ((Ore 3) (Clay 8)))
        (Geode ((Ore 3) (Obsidian 12))))))) |}]

module Simulation = struct
  type state = {
    blueprint : Blueprint.t;
    robots : ResourceSet.t;
    resources : ResourceSet.t;
  }
  [@@deriving sexp, equal, compare, hash]

  module State_key = struct
    module T = struct
      type t = state [@@deriving sexp, equal, compare, hash]
    end

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)
  end

  let init blueprint =
    {
      blueprint;
      robots = ResourceSet.uniform Ore 1;
      resources = ResourceSet.empty;
    }

  let execute_purchases blueprint purchases =
    let try_purchase state robot =
      let cost = Map.find_exn state.blueprint.costs robot in
      ResourceSet.sub state.resources cost
      |> Option.map ~f:(fun resources ->
             let robots =
               ResourceSet.add state.robots (ResourceSet.uniform robot 1)
             in
             { state with robots; resources })
    in
    let rec loop state step ps =
      print_s
        [%message
          (state.resources : ResourceSet.t) (state.robots : ResourceSet.t)];
      if step >= 24 then state
      else
        let new_resources = state.robots in
        let state', ps' =
          match ps with
          | [] -> (state, [])
          | robot :: ps -> (
              match try_purchase state robot with
              | None -> (state, robot :: ps)
              | Some state' -> (state', ps))
        in
        loop
          {
            state' with
            resources = ResourceSet.add state'.resources new_resources;
          }
          (step + 1) ps'
    in
    let state = loop (init blueprint) 0 purchases in
    Map.find state.resources Geode |> Option.value ~default:0
end

let part1 input =
  input |> String.split_lines
  |> List.map ~f:Blueprint.of_string
  |> List.map ~f:(fun bp ->
         Simulation.execute_purchases bp
           [ Clay; Clay; Clay; Obsidian; Clay; Obsidian; Geode; Geode ])
  |> [%sexp_of: int list] |> Sexp.to_string_hum
(* |> List.sum (module Int) ~f:(fun b -> Simulation.maximize_blueprint b * b.id)
   |> Int.to_string *)

let%expect_test "part1" =
  let result = part1 example in
  print_endline result;
  [%expect {|  |}]

let part2 _ = ""

let%expect_test "part2" =
  let result = part2 example in
  print_endline result;
  [%expect {|  |}]
