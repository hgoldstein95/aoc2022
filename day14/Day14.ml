open! Core
open! Stdio

let example = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"

type point = { x : int; y : int } [@@deriving sexp, equal, compare]

module Point = struct
  module T = struct
    type t = point [@@deriving sexp, equal, compare]
  end

  include T

  let on_line p1 p p2 =
    let lo, hi = if [%compare: point] p1 p2 <= 0 then (p1, p2) else (p2, p1) in
    (lo.x = hi.x && p.x = lo.x && lo.y <= p.y && p.y <= hi.y)
    || (lo.y = hi.y && p.y = lo.y && lo.x <= p.x && p.x <= hi.x)

  let%test_unit "between" =
    let open Quickcheck.Let_syntax in
    let module G = Quickcheck.Generator in
    let gen_x = Int.gen_incl (-100) 100 in
    let gen_y = Int.gen_incl 1 1000 in
    let gen_horizontal =
      let%bind x1 = gen_x in
      let%bind x2 = gen_x in
      let%map y = gen_y in
      ({ x = x1; y }, { x = x2; y })
    in
    let gen_vertical =
      let%bind y1 = gen_y in
      let%bind y2 = gen_y in
      let%map x = gen_x in
      ({ x; y = y1 }, { x; y = y2 })
    in
    (* EndPoint.ts are OK *)
    Quickcheck.test
      (G.union [ gen_horizontal; gen_vertical ])
      ~sexp_of:[%sexp_of: t * t]
      ~f:(fun (p1, p2) -> assert (on_line p1 p1 p2 && on_line p1 p2 p2));
    (* Just out of bounds is NOT OK *)
    Quickcheck.test
      (G.union [ gen_horizontal; gen_vertical ])
      ~sexp_of:[%sexp_of: t * t]
      ~f:(fun (p1, p2) ->
        let lo, hi = if [%compare: t] p1 p2 <= 0 then (p1, p2) else (p2, p1) in
        assert (not @@ on_line p1 { x = lo.x; y = lo.y - 1 } p2);
        assert (not @@ on_line p1 { x = lo.x - 1; y = lo.y } p2);
        assert (not @@ on_line p1 { x = hi.x + 1; y = hi.y } p2);
        assert (not @@ on_line p1 { x = hi.x; y = hi.y + 1 } p2))

  let all_between p1 p2 =
    if p1.x = p2.x then
      List.range (min p1.y p2.y) (max p1.y p2.y) ~start:`inclusive
        ~stop:`inclusive
      |> List.map ~f:(fun y -> { x = p1.x; y })
    else
      List.range (min p1.x p2.x) (max p1.x p2.x) ~start:`inclusive
        ~stop:`inclusive
      |> List.map ~f:(fun x -> { x; y = p1.y })

  include Comparable.Make (T)
end

module Path = struct
  type t = Point.t list [@@deriving sexp, equal, compare]

  let of_string (s : string) : t =
    let open Angstrom in
    let int =
      Int.of_string <$> take_while1 (function '0' .. '9' -> true | _ -> false)
    in
    let point = (fun x _ y -> { x; y }) <$> int <*> char ',' <*> int in
    let points = sep_by (string " -> ") point in
    match parse_string ~consume:All points s with
    | Ok v -> v
    | Error msg -> failwith msg
end

module Rocks = struct
  type t = Set.M(Point).t [@@deriving sexp, equal, compare]

  let of_string (s : string) : t =
    let paths = s |> String.split_lines |> List.map ~f:Path.of_string in
    List.concat_map paths ~f:(fun path ->
        List.zip_with_remainder path (List.tl path |> Option.value ~default:[])
        |> fst
        |> List.concat_map ~f:(fun (p1, p2) -> Point.all_between p1 p2))
    |> Set.of_list (module Point)

  let limit (rocks : t) =
    rocks |> Set.map (module Int) ~f:(fun { x = _; y } -> y) |> Set.max_elt_exn

  let is_rock (rocks : t) ~(point : Point.t) = Set.mem rocks point
end

module Sand = struct
  type t = {
    rocks : Rocks.t;
    rock_limit : int;
    sand : Point.t list;
    sand_source : Point.t list;
  }
  [@@deriving sexp, equal, compare, fields]

  let init rocks =
    let rock_limit = Rocks.limit rocks in
    { rocks; rock_limit; sand = []; sand_source = [ { x = 500; y = 0 } ] }

  let drop_sand ~floor state =
    let no_object point =
      let is_rock = Rocks.is_rock state.rocks ~point in
      let is_sand = List.mem ~equal:[%equal: Point.t] state.sand point in
      not (is_rock || is_sand)
    in
    let on_floor point = point.y + 1 = state.rock_limit + 2 in
    let out_of_bounds point = point.y > state.rock_limit in
    let rec loop = function
      | curr :: stack ->
          let below = { x = curr.x; y = curr.y + 1 } in
          let below_left = { x = curr.x - 1; y = curr.y + 1 } in
          let below_right = { x = curr.x + 1; y = curr.y + 1 } in
          if floor && on_floor curr then Some (curr, stack)
          else if out_of_bounds curr then None
          else if no_object below then loop (below :: curr :: stack)
          else if no_object below_left then loop (below_left :: curr :: stack)
          else if no_object below_right then loop (below_right :: curr :: stack)
          else Some (curr, stack)
      | [] -> None
    in
    Option.map (loop state.sand_source) ~f:(fun (grain, stack) ->
        { state with sand = grain :: state.sand; sand_source = stack })

  (* let _display (state : t) : string =
     let all_points =
       Set.union state.rocks (Set.of_list (module Point) state.sand)
     in
     let x_min, x_max =
       let xs = Set.map (module Int) ~f:(fun { x; y = _ } -> x) all_points in
       (Set.min_elt_exn xs, Set.max_elt_exn xs)
     in
     let y_min, y_max =
       let ys = Set.map (module Int) ~f:(fun { x = _; y } -> y) all_points in
       (0, Set.max_elt_exn ys)
     in
     List.range ~start:`inclusive ~stop:`inclusive y_min y_max
     |> List.map ~f:(fun y ->
            List.range ~start:`inclusive ~stop:`inclusive x_min x_max
            |> List.map ~f:(fun x ->
                   if List.mem state.sand { x; y } ~equal:Point.equal then 'o'
                   else if Set.mem state.rocks { x; y } then '#'
                   else '.')
            |> String.of_char_list)
     |> String.concat ~sep:"\n" *)

  let display_notty (state : t) : Notty.image =
    let open Notty in
    let sand = I.string A.(fg lightred) "o" in
    let rock = I.string A.(fg lightred) "#" in
    (* let sand_source = I.string A.(fg lightred) "+" in *)
    let nothing = I.string A.(fg lightred) " " in
    let all_points =
      Set.union state.rocks (Set.of_list (module Point) state.sand)
    in
    let x_min, x_max =
      let xs = Set.map (module Int) ~f:(fun { x; y = _ } -> x) all_points in
      (Set.min_elt_exn xs, Set.max_elt_exn xs)
    in
    let y_min, y_max =
      let ys = Set.map (module Int) ~f:(fun { x = _; y } -> y) all_points in
      (0, Set.max_elt_exn ys)
    in
    I.tabulate (x_max - x_min) (y_max - y_min) (fun i j ->
        let x, y = (i + x_min, j + y_min) in
        let point = { x; y } in
        if List.mem state.sand point ~equal:[%equal: Point.t] then sand
        else if Set.mem state.rocks point then rock (* TODO Sand source *)
        else nothing)

  let simulate ?(floor = false) state =
    let rec loop s =
      match drop_sand ~floor s with Some s' -> loop s' | None -> s
    in
    loop state

  let visualize ?(floor = false) state =
    let open Lwt.Infix in
    let open Notty_lwt in
    let open Notty in
    let timer () = Lwt_unix.sleep 0.001 >|= fun () -> `Timer in
    let event term =
      Lwt_stream.get (Term.events term) >|= function
      | Some ((`Resize _ | #Unescape.event) as x) -> x
      | None -> `End
    in

    let terminal = Term.create () in
    let rec loop term (e, t) s =
      e <?> t >>= function
      | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [ `Ctrl ]) ->
          Lwt.return_unit
      | `Timer -> (
          Term.image term (display_notty s) >>= fun () ->
          match drop_sand ~floor s with
          | Some s' -> loop term (e, timer ()) s'
          | None -> Lwt.return_unit)
      | _ -> loop term (event term, t) s
    in
    Lwt_main.run @@ loop terminal (event terminal, timer ()) state
end

let%expect_test "Path.of_string" =
  let paths = example |> String.split_lines |> List.map ~f:Path.of_string in
  print_s [%sexp (paths : Path.t list)];
  [%expect
    {|
    ((((x 498) (y 4)) ((x 498) (y 6)) ((x 496) (y 6)))
     (((x 503) (y 4)) ((x 502) (y 4)) ((x 502) (y 9)) ((x 494) (y 9)))) |}]

let part1 input =
  input |> Rocks.of_string |> Sand.init |> Sand.simulate |> Sand.sand
  |> List.length |> Int.to_string

let%expect_test "part1" =
  let result = part1 example in
  print_endline result;
  [%expect {| 24 |}]

let part2 input =
  input |> Rocks.of_string |> Sand.init |> Sand.simulate ~floor:true
  |> Sand.sand |> List.length |> Int.to_string

let part2_vis input =
  input |> Rocks.of_string |> Sand.init |> Sand.visualize ~floor:true

let%expect_test "part2" =
  let result = part2 example in
  print_endline result;
  [%expect {| 93 |}]
