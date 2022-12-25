open! Core
open! Stdio

let example =
  "2,2,2\n\
   1,2,2\n\
   3,2,2\n\
   2,1,2\n\
   2,3,2\n\
   2,2,1\n\
   2,2,3\n\
   2,2,4\n\
   2,2,6\n\
   1,2,5\n\
   3,2,5\n\
   2,1,5\n\
   2,3,5"

module Point = struct
  module T = struct
    type t = { x : int; y : int; z : int } [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let of_string (s : string) : t =
    match String.split s ~on:',' with
    | [ x; y; z ] ->
        { x = Int.of_string x; y = Int.of_string y; z = Int.of_string z }
    | _ -> failwith "invalid input"

  let neighbors (point : t) : t list =
    let open List.Let_syntax in
    let%bind dir = [ `X; `Y; `Z ] in
    let%map sign = [ 1; -1 ] in
    match dir with
    | `X -> { point with x = point.x + sign }
    | `Y -> { point with y = point.y + sign }
    | `Z -> { point with z = point.z + sign }

  let apply_delta (point : t) (delta : int) (dir : [ `X | `Y | `Z ]) : t =
    match dir with
    | `X -> { point with x = point.x + delta }
    | `Y -> { point with y = point.y + delta }
    | `Z -> { point with z = point.z + delta }
end

module Droplet = struct
  type t = Hash_set.M(Point).t [@@deriving sexp]

  let of_string (s : string) : t =
    s |> String.split_lines
    |> List.map ~f:Point.of_string
    |> Hash_set.of_list (module Point)

  let interior_surface_area (droplet : t) : int =
    (let open List.Let_syntax in
    let%bind point = Hash_set.to_list droplet in
    let%bind neighbor = Point.neighbors point in
    if Hash_set.mem droplet neighbor then [] else [ neighbor ])
    |> List.length

  let exterior_surface_area (droplet : t) : int =
    let module Face = struct
      module T = struct
        type t = [ `X | `Y | `Z ] * int * Point.t
        [@@deriving compare, hash, sexp, equal]
      end

      include T
      include Comparable.Make (T)
      include Hashable.Make (T)
    end in
    let face_neighbors (face : Face.t) : Face.t list =
      let other_dirs = function
        | `X -> [ `Y; `Z ]
        | `Y -> [ `X; `Z ]
        | `Z -> [ `X; `Y ]
      in
      let all_signs = [ 1; -1 ] in

      let open List.Let_syntax in
      let dir, sign, point = face in
      let adjoining =
        let%bind d = other_dirs dir in
        let%map s = all_signs in
        (d, s, point)
      in
      let in_plane =
        let%bind d = other_dirs dir in
        let%map s = all_signs in
        (dir, sign, Point.apply_delta point s d)
      in
      let adjacent =
        let%bind d = other_dirs dir in
        let%map s = all_signs in
        (d, -s, Point.apply_delta (Point.apply_delta point sign dir) s d)
      in
      adjoining @ in_plane @ adjacent
    in
    let is_surface face =
      let dir, sign, point = face in
      Hash_set.mem droplet point
      && not (Hash_set.mem droplet (Point.apply_delta point sign dir))
    in
    let is_blocked source target =
      let source_dir, source_sign, source_point = source in
      let target_dir, target_sign, target_point = target in
      [%equal: Point.t] source_point target_point
      &&
      let other_point =
        Point.apply_delta
          (Point.apply_delta source_point source_sign source_dir)
          target_sign target_dir
      in
      Hash_set.mem droplet other_point
    in
    let visited = Hash_set.create (module Face) in
    let rec loop = function
      | [] -> ()
      | face :: queue ->
          Hash_set.add visited face;
          let neighbors =
            face_neighbors face
            |> List.filter ~f:(fun f ->
                   is_surface f
                   && (not (is_blocked face f))
                   && (not (Hash_set.mem visited f))
                   && not (List.mem queue f ~equal:Face.equal))
          in
          loop (queue @ neighbors)
    in
    (* FIXME: This only works because the connected components happen to all be
       visible from Y+. Ideally, we should pick a list of starting points that
       necessarily starts at least once on each connected components. *)
    let starts =
      droplet |> Hash_set.to_list
      |> List.sort_and_group ~compare:(fun p1 p2 ->
             [%compare: int * int] (p1.Point.x, p1.Point.z)
               (p2.Point.x, p2.Point.z))
      |> List.map ~f:(fun l ->
             l
             |> List.max_elt ~compare:(fun p1 p2 ->
                    Int.compare p1.Point.y p2.Point.y)
             |> Option.value_exn
             |> fun p -> (`Y, 1, p))
    in
    let queue = starts in
    loop queue;
    Hash_set.length visited
end

let part1 input =
  input |> Droplet.of_string |> Droplet.interior_surface_area |> Int.to_string

let%expect_test "part1" =
  let result = part1 example in
  print_endline result;
  [%expect {| 64 |}]

let part2 input =
  input |> Droplet.of_string |> Droplet.exterior_surface_area |> Int.to_string

let%expect_test "part2" =
  let result = part2 example in
  print_endline result;
  [%expect {| 58 |}]
