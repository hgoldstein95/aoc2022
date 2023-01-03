open! Core

type t = [ `Face of int | `Empty ] array array [@@deriving sexp, equal]

let of_string s =
  s |> String.split_lines
  |> List.map ~f:String.to_array
  |> List.map
       ~f:
         (Array.map ~f:(function
           | ' ' -> `Empty
           | '0' .. '9' as c -> `Face (Char.to_int c - Char.to_int '0')
           | _ -> failwith "invalid input"))
  |> Array.of_list

let%expect_test "of_string" =
  let n = of_string "123  \n  456" in
  print_s [%sexp (n : t)];
  [%expect
    {|
    (((Face 1) (Face 2) (Face 3) Empty Empty)
     (Empty Empty (Face 4) (Face 5) (Face 6))) |}]

let is_valid (n : t) : bool =
  let width_invariant =
    let width_0 = Array.length n.(0) in
    Array.for_all n ~f:(fun row -> Array.length row = width_0)
  in
  let face_invariant =
    let faces =
      n |> Array.to_list |> Array.concat |> Array.to_list
      |> List.filter_map ~f:(function `Face x -> Some x | `Empty -> None)
      |> List.sort ~compare:Int.compare
    in
    [%equal: int list] faces [ 1; 2; 3; 4; 5; 6 ]
  in
  width_invariant && face_invariant

let fundamentals =
  List.map ~f:of_string
    [
      "  156\n324  ";
      "14  \n 26 \n  35";
      "  12\n354 \n6   ";
      "  12\n354 \n 6  ";
      "1   \n3245\n6   ";
      " 1  \n3245\n 6  ";
      " 1  \n3245\n  6 ";
      "1   \n3245\n   6";
      "1   \n3245\n 6  ";
      "1   \n3245\n  6 ";
      "  12\n354 \n  6 ";
    ]

let%test_unit "fundamentals are valid" =
  List.iter fundamentals ~f:(fun n -> assert (is_valid n))

let flip (n : t) : t = Array.map n ~f:(fun row -> Array.rev row)

let rotate (n : t) : t =
  let width = Array.length n.(0) in
  let height = Array.length n in
  Array.init width ~f:(fun x ->
      Array.init height ~f:(fun y -> n.(height - y - 1).(x)))

let%expect_test "rotate" =
  let n = of_string "123  \n  456" in
  print_s [%sexp (n : t)];
  print_s [%sexp (rotate n : t)];
  [%expect
    {|
    (((Face 1) (Face 2) (Face 3) Empty Empty)
     (Empty Empty (Face 4) (Face 5) (Face 6)))
    ((Empty (Face 1)) (Empty (Face 2)) ((Face 4) (Face 3)) ((Face 5) Empty)
     ((Face 6) Empty)) |}]

let all : t list =
  let open List.Let_syntax in
  let%bind do_flip = [ false; true ] in
  let%bind rotation = [ 0; 1; 2; 3 ] in
  let%map fundamental = fundamentals in
  (if do_flip then flip else Fn.id)
    (Fn.apply_n_times ~n:rotation rotate fundamental)

let%expect_test "all" =
  print_endline (Int.to_string (List.length all));
  [%expect {| 88 |}]
