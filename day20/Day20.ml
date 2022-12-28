open! Core
open! Stdio

let example = "1\n2\n-3\n3\n-2\n0\n4"
let decryption_key = 811589153

module SwapRing = struct
  module Elt = struct
    type t = int Doubly_linked.Elt.t

    let sexp_of_t t = [%sexp (Doubly_linked.Elt.value t : int)]
    let value (e : t) : int = Doubly_linked.Elt.value e
  end

  type t = { ring : int Doubly_linked.t; index : Elt.t array }
  [@@deriving sexp_of]

  exception Empty_ring

  let length r = Array.length r.index

  let of_list (xs : int list) : t =
    let ring = Doubly_linked.of_list xs in
    let curr = ref (Doubly_linked.first_elt ring) in
    let index =
      Array.create ~len:(List.length xs) (!curr |> Option.value_exn)
    in
    let i = ref 0 in
    while not (Option.is_none !curr) do
      let curr_val = !curr |> Option.value_exn in
      index.(!i) <- curr_val;
      curr := Doubly_linked.next ring curr_val;
      incr i
    done;
    { ring; index }

  let first_elt (r : t) : Elt.t =
    match Doubly_linked.first_elt r.ring with
    | None -> raise Empty_ring
    | Some v -> v

  let last_elt (r : t) : Elt.t =
    match Doubly_linked.last_elt r.ring with
    | None -> raise Empty_ring
    | Some v -> v

  let next (r : t) (e : Elt.t) : Elt.t =
    Option.value (Doubly_linked.next r.ring e) ~default:(first_elt r)

  let prev (r : t) (e : Elt.t) : Elt.t =
    Option.value (Doubly_linked.prev r.ring e) ~default:(last_elt r)

  let move_after (r : t) (e : Elt.t) ~(anchor : Elt.t) : unit =
    if Doubly_linked.is_last r.ring anchor then
      Doubly_linked.move_to_front r.ring e
    else Doubly_linked.move_after r.ring e ~anchor

  let move_before (r : t) (e : Elt.t) ~(anchor : Elt.t) : unit =
    if Doubly_linked.is_first r.ring anchor then
      Doubly_linked.move_to_back r.ring e
    else Doubly_linked.move_before r.ring e ~anchor

  let swap (r : t) (e : Elt.t) (i : int) : unit =
    let fwd = i >= 0 in
    let i = abs i % (length r - 1) in
    if i = 0 then ()
    else
      let curr = ref e in
      Fn.apply_n_times ~n:i
        (fun _ -> curr := if fwd then next r !curr else prev r !curr)
        ();
      if fwd then move_after r e ~anchor:!curr
      else move_before r e ~anchor:!curr

  let mix (r : t) : unit =
    Array.iter r.index ~f:(fun e -> swap r e (Elt.value e))

  let%expect_test "mix" =
    let ring = of_list [ 0; 1; 2 ] in
    mix ring;
    print_s ([%sexp_of: t] ring);
    [%expect {| ((ring (1 0 2)) (index (0 1 2))) |}];
    let ring = of_list [ 0; 1; -2 ] in
    mix ring;
    print_s ([%sexp_of: t] ring);
    [%expect {| ((ring (1 0 -2)) (index (0 1 -2))) |}];
    let ring = of_list [ 0; 1; 5 ] in
    mix ring;
    print_s ([%sexp_of: t] ring);
    [%expect {| ((ring (1 5 0)) (index (0 1 5))) |}]

  let to_array (r : t) : int array = Doubly_linked.to_array r.ring
end

let part1 input =
  let ring =
    input |> String.split_lines |> List.map ~f:Int.of_string |> SwapRing.of_list
  in
  SwapRing.mix ring;
  let arr = SwapRing.to_array ring in
  let i_0, _ = Array.findi_exn arr ~f:(fun _ x -> x = 0) in
  List.map [ 1000; 2000; 3000 ] ~f:(fun offset ->
      arr.((i_0 + offset) % Array.length arr))
  |> List.sum (module Int) ~f:Fn.id
  |> Int.to_string

let%expect_test "part1" =
  let result = part1 example in
  print_endline result;
  [%expect {| 3 |}]

let part2 input =
  let ring =
    input |> String.split_lines |> List.map ~f:Int.of_string
    |> List.map ~f:(fun x -> x * decryption_key)
    |> SwapRing.of_list
  in
  Fn.apply_n_times ~n:10 (fun _ -> SwapRing.mix ring) ();
  let arr = SwapRing.to_array ring in
  let i_0, _ = Array.findi_exn arr ~f:(fun _ x -> x = 0) in
  List.map [ 1000; 2000; 3000 ] ~f:(fun offset ->
      arr.((i_0 + offset) % Array.length arr))
  |> List.sum (module Int) ~f:Fn.id
  |> Int.to_string

let%expect_test "part2" =
  let result = part2 example in
  print_endline result;
  [%expect {| 1623178306 |}]
