open! Core
open! Stdio

let example = "30373\n25512\n65332\n33549\n35390"

module Forest = struct
  type t = { width : int; height : int; tree_array : int array array }
  [@@deriving sexp_of]

  let get ~(x : int) ~(y : int) (forest : t) : int = forest.tree_array.(y).(x)

  let of_string (s : string) : t =
    let lines = String.split_lines s in
    let height = List.length lines in
    let width = String.length (List.hd_exn lines) in
    let tree_array =
      lines
      |> List.map ~f:(fun s ->
             s |> String.to_array |> Array.map ~f:Char.get_digit_exn)
      |> Array.of_list
    in
    { width; height; tree_array }

  let is_visible (forest : t) (x : int) (y : int) : bool =
    let in_direction x y dx dy =
      let rec loop acc x y =
        if x < 0 || y < 0 || x > forest.width - 1 || y > forest.height - 1 then
          acc
        else loop (get forest ~x ~y :: acc) (x + dx) (y + dy)
      in
      loop [] (x + dx) (y + dy)
    in
    let above = in_direction x y 0 (-1) in
    let below = in_direction x y 0 1 in
    let left = in_direction x y (-1) 0 in
    let right = in_direction x y 1 0 in
    let tree = get forest ~x ~y in
    List.exists
      ~f:(fun dir -> List.for_all dir ~f:(fun other -> other < tree))
      [ above; below; left; right ]

  let%test_unit "is_visible" =
    let forest = of_string "050\n515\n050" in
    assert (is_visible forest 0 0);
    assert (not (is_visible forest 1 1))

  let count_visible_from_outside (forest : t) : int =
    let count = ref 0 in
    for x = 0 to forest.width - 1 do
      for y = 0 to forest.height - 1 do
        if is_visible forest x y then incr count
      done
    done;
    !count

  let%expect_test "count_visible_from_outside" =
    let forest = of_string "050\n515\n050" in
    print_s [%sexp (count_visible_from_outside forest : int)];
    [%expect {| 8 |}]

  let scenic_score (forest : t) ~(x : int) ~(y : int) : int =
    let in_direction x y dx dy =
      let here = get forest ~x ~y in
      let rec loop acc x y =
        if x < 0 || y < 0 || x > forest.width - 1 || y > forest.height - 1 then
          acc
        else if get forest ~x ~y >= here then 1 + acc
        else loop (1 + acc) (x + dx) (y + dy)
      in
      loop 0 (x + dx) (y + dy)
    in
    let above = in_direction x y 0 (-1) in
    let below = in_direction x y 0 1 in
    let left = in_direction x y (-1) 0 in
    let right = in_direction x y 1 0 in
    above * below * left * right
end

let%expect_test "Forest.of_string" =
  let forest = Forest.of_string example in
  print_s [%sexp (forest : Forest.t)];
  [%expect
    {|
    ((width 5) (height 5)
     (tree_array ((3 0 3 7 3) (2 5 5 1 2) (6 5 3 3 2) (3 3 5 4 9) (3 5 3 9 0)))) |}]

let%test_unit "Forest.is_visible" =
  let forest = Forest.of_string example in
  assert (Forest.is_visible forest 0 0);
  assert (not (Forest.is_visible forest 2 2))

let part1 input =
  let forest = Forest.of_string input in
  forest |> Forest.count_visible_from_outside |> Int.to_string

let%expect_test "part1" =
  let result = part1 example in
  print_endline result;
  [%expect {| 21 |}]

let%expect_test "Forest.scenic_score" =
  let forest = Forest.of_string example in
  print_s [%sexp (forest |> Forest.scenic_score ~x:2 ~y:1 : int)];
  [%expect {| 4 |}];
  print_s [%sexp (forest |> Forest.scenic_score ~x:2 ~y:3 : int)];
  [%expect {| 8 |}]

let part2 input =
  let forest = Forest.of_string input in
  let max_score = ref Int.min_value in
  for x = 0 to forest.width - 1 do
    for y = 0 to forest.height - 1 do
      let score = Forest.scenic_score forest ~x ~y in
      if score > !max_score then max_score := score
    done
  done;
  Int.to_string !max_score

let%expect_test "part2" =
  let result = part2 example in
  print_endline result;
  [%expect {| 8 |}]
