open! Core
open! Stdio

let example = "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"

module Loc = struct
  module T = struct
    type t = { x : int; y : int }
    [@@deriving sexp_of, equal, compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)

  let create ~x ~y = { x; y }
  let x t = t.x
  let y t = t.y

  let neighbors t =
    [
      { t with x = t.x - 1 };
      { t with x = t.x + 1 };
      { t with y = t.y - 1 };
      { t with y = t.y + 1 };
    ]
end

module ElevationGrid = struct
  module Grid = struct
    type 'a t = 'a array array [@@deriving sexp_of, equal, compare]

    let get t loc = t.(Loc.y loc).(Loc.x loc)
    let put t loc v = t.(Loc.y loc).(Loc.x loc) <- v

    let in_bounds t loc =
      Loc.x loc >= 0
      && Loc.y loc >= 0
      && Loc.x loc < Array.length t.(0)
      && Loc.y loc < Array.length t

    let to_list t =
      Array.mapi t ~f:(fun y row ->
          Array.mapi row ~f:(fun x v -> (Loc.create ~x ~y, v)) |> Array.to_list)
      |> Array.to_list |> List.concat
  end

  type t = int Grid.t [@@deriving sexp_of]

  let of_string s =
    let start = ref (Loc.create ~x:0 ~y:0) in
    let destination = ref (Loc.create ~x:0 ~y:0) in
    let grid =
      String.split_lines s
      |> List.mapi ~f:(fun y line ->
             String.to_array line
             |> Array.mapi ~f:(fun x -> function
                  | 'S' ->
                      start := { x; y };
                      0
                  | 'E' ->
                      destination := { x; y };
                      25
                  | c -> Char.to_int c - Char.to_int 'a'))
      |> Array.of_list
    in
    (!start, !destination, grid)

  let neighbors t loc =
    let open List.Let_syntax in
    let%bind loc' = Loc.neighbors loc in
    if Grid.in_bounds t loc' && Grid.get t loc' - Grid.get t loc <= 1 then
      [ loc' ]
    else []

  (* Dijkstra's Algorithm for Single-Source Shortest Path *)
  let shortest_path t ~start ~destination =
    let module IntLocPQ = PriorityQueue.M (Int) (Loc) in
    let state = Array.map ~f:(Array.map ~f:(fun _ -> (`Unvisited, None))) t in
    let pqueue =
      IntLocPQ.empty |> IntLocPQ.insert ~value:start ~priority:0 |> ref
    in
    let finished = ref false in

    Grid.put state start (`Unvisited, Some 0);
    while not !finished do
      match IntLocPQ.extract !pqueue with
      | Some (dist, loc, pqueue') ->
          pqueue := pqueue';
          List.iter (neighbors t loc) ~f:(fun n ->
              let v, old_dist = Grid.get state n in
              match v with
              | `Visited -> ()
              | `Unvisited ->
                  let n_dist =
                    match old_dist with
                    | None -> dist + 1
                    | Some x -> min (dist + 1) x
                  in
                  Grid.put state n (`Unvisited, Some n_dist);
                  pqueue := IntLocPQ.insert !pqueue ~value:n ~priority:n_dist);
          Grid.put state loc (`Visited, Some dist);
          if [%equal: Loc.t] loc destination then finished := true
      | None -> finished := true
    done;
    Grid.get state destination |> snd
end

let%expect_test "ElevationGrid.of_string" =
  let result = ElevationGrid.of_string example in
  print_s [%sexp (result : Loc.t * Loc.t * ElevationGrid.t)];
  [%expect
    {|
    (((x 0) (y 0)) ((x 5) (y 2))
     ((0 0 1 16 15 14 13 12) (0 1 2 17 24 23 23 11) (0 2 2 18 25 25 23 10)
      (0 2 2 19 20 21 22 9) (0 1 3 4 5 6 7 8))) |}]

let part1 input =
  let start, destination, grid = ElevationGrid.of_string input in
  ElevationGrid.shortest_path grid ~start ~destination
  |> Option.value_exn |> Int.to_string

let%expect_test "part1" =
  let result = part1 example in
  print_endline result;
  [%expect {| 31 |}]

let part2 input =
  let _, destination, grid = ElevationGrid.of_string input in
  ElevationGrid.Grid.to_list grid
  |> List.map ~f:(fun (loc, x) ->
         if x = 0 then ElevationGrid.shortest_path grid ~start:loc ~destination
         else None)
  |> List.filter_opt
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn |> Int.to_string

let%expect_test "part2" =
  let result = part2 example in
  print_endline result;
  [%expect {| 29 |}]
