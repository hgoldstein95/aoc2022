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
end

module ResourceSet : sig
  type t [@@deriving sexp, equal, compare, hash]

  val empty : t
  val uniform : Resource.t -> int -> t
  val add : t -> t -> t
  val sub : t -> t -> t option
  val scale : t -> int -> t
  val get_cost_list : t -> (Resource.t * int) list
  val find : t -> Resource.t -> int

  include Comparable.S with type t := t
end = struct
  module T = struct
    type t = { ore : int; clay : int; obsidian : int; geode : int }
    [@@deriving sexp, equal, compare, hash]
  end

  include T

  let empty = { ore = 0; clay = 0; obsidian = 0; geode = 0 }

  let uniform r n =
    assert (n >= 0);
    match r with
    | Resource.Ore -> { empty with ore = n }
    | Resource.Clay -> { empty with clay = n }
    | Resource.Obsidian -> { empty with obsidian = n }
    | Resource.Geode -> { empty with geode = n }

  let add (a : t) (b : t) : t =
    {
      ore = a.ore + b.ore;
      clay = a.clay + b.clay;
      obsidian = a.obsidian + b.obsidian;
      geode = a.geode + b.geode;
    }

  let sub (a : t) (b : t) : t option =
    if
      a.ore < b.ore || a.clay < b.clay || a.obsidian < b.obsidian
      || a.geode < b.geode
    then None
    else
      Some
        {
          ore = a.ore - b.ore;
          clay = a.clay - b.clay;
          obsidian = a.obsidian - b.obsidian;
          geode = a.geode - b.geode;
        }

  let scale a n =
    assert (n >= 0);
    {
      ore = a.ore * n;
      clay = a.clay * n;
      obsidian = a.obsidian * n;
      geode = a.geode * n;
    }

  let get_cost_list t =
    Resource.
      [ (Ore, t.ore); (Clay, t.clay); (Obsidian, t.obsidian); (Geode, t.geode) ]
    |> List.filter ~f:(fun (_, n) -> n > 0)

  let find t = function
    | Resource.Ore -> t.ore
    | Resource.Clay -> t.clay
    | Resource.Obsidian -> t.obsidian
    | Resource.Geode -> t.geode

  include Comparable.Make (T)
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
       ((Ore ((ore 4) (clay 0) (obsidian 0) (geode 0)))
        (Clay ((ore 2) (clay 0) (obsidian 0) (geode 0)))
        (Obsidian ((ore 3) (clay 14) (obsidian 0) (geode 0)))
        (Geode ((ore 2) (clay 0) (obsidian 7) (geode 0))))))
     ((id 2)
      (costs
       ((Ore ((ore 2) (clay 0) (obsidian 0) (geode 0)))
        (Clay ((ore 3) (clay 0) (obsidian 0) (geode 0)))
        (Obsidian ((ore 3) (clay 8) (obsidian 0) (geode 0)))
        (Geode ((ore 3) (clay 0) (obsidian 12) (geode 0))))))) |}]

module Simulation = struct
  type state = {
    blueprint : Blueprint.t;
    robots : ResourceSet.t;
    resources : ResourceSet.t;
    step : int;
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
      step = 0;
    }

  let accumulate_for state ~n =
    assert (n >= 0);
    {
      state with
      step = state.step + n;
      resources = ResourceSet.(add state.resources (scale state.robots n));
    }

  let steps_to_build state ~target_robot =
    let costs =
      Map.find_exn state.blueprint.costs target_robot
      |> ResourceSet.get_cost_list
    in
    let potential_steps =
      List.map costs ~f:(fun (resource, cost) ->
          let stock = ResourceSet.find state.resources resource in
          let n_robots = ResourceSet.find state.robots resource in
          if stock >= cost then Some 0
          else if n_robots = 0 then None
          else Some ((cost - stock + n_robots - 1) / n_robots))
      |> Option.all
    in
    match potential_steps with
    | None -> `Not_enough_robots
    | Some potential_steps ->
        let steps =
          potential_steps
          |> List.max_elt ~compare:Int.compare
          |> Option.value_exn
        in
        assert (steps >= 0);
        `Steps steps

  let build_robot state ~target_robot =
    let costs = Map.find_exn state.blueprint.costs target_robot in
    {
      state with
      resources = ResourceSet.sub state.resources costs |> Option.value_exn;
      robots = ResourceSet.(add state.robots (uniform target_robot 1));
    }

  let maximize_blueprint blueprint ~total_steps =
    let loop self state =
      let targets =
        Resource.[ Geode; Obsidian; Clay; Ore ]
        |> List.filter_map ~f:(fun target_robot ->
               match steps_to_build state ~target_robot with
               | `Not_enough_robots -> None
               | `Steps steps when state.step + steps + 1 > total_steps -> None
               | `Steps steps -> Some (target_robot, steps))
      in
      match targets with
      | [] ->
          let final_state =
            accumulate_for state ~n:(total_steps - state.step)
          in
          ResourceSet.find final_state.resources Geode
      | _ ->
          List.map targets ~f:(fun (target_robot, steps) ->
              state
              |> accumulate_for ~n:(steps + 1)
              |> build_robot ~target_robot |> self)
          |> List.max_elt ~compare:Int.compare
          |> Option.value_exn
    in
    (* let cache = Hashtbl.create (module State_key) in *)
    let rec fix f x =
      (* match Hashtbl.find cache x with
         | Some y -> y
         | None -> *)
      let y = f (fix f) x in
      (* Hashtbl.set cache ~key:x ~data:y; *)
      y
    in
    fix loop (init blueprint)
end

let part1 input =
  input |> String.split_lines
  |> List.map ~f:Blueprint.of_string
  |> List.sum
       (module Int)
       ~f:(fun b -> Simulation.maximize_blueprint b ~total_steps:24 * b.id)
  |> Int.to_string

let%expect_test "part1" =
  let result = part1 example in
  print_endline result;
  [%expect {| 33 |}]

let part2 input =
  input |> String.split_lines |> Fn.flip List.take 3
  |> List.map ~f:Blueprint.of_string
  |> List.map ~f:(fun b -> Simulation.maximize_blueprint b ~total_steps:32)
  |> List.reduce_exn ~f:(fun x y -> x * y)
  |> Int.to_string

let%expect_test "part2" =
  let result = part2 example in
  print_endline result;
  [%expect {| 3472 |}]
