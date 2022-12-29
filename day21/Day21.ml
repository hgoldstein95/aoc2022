open! Core
open! Stdio

let example =
  "root: pppw + sjmn\n\
   dbpl: 5\n\
   cczh: sllz + lgvd\n\
   zczc: 2\n\
   ptdq: humn - dvpt\n\
   dvpt: 3\n\
   lfqf: 4\n\
   humn: 5\n\
   ljgn: 2\n\
   sjmn: drzm * dbpl\n\
   sllz: 4\n\
   pppw: cczh / lfqf\n\
   lgvd: ljgn * ptdq\n\
   drzm: hmdt - zczc\n\
   hmdt: 32"

module Monkeys = struct
  exception Invalid_root
  exception Parse_error of [ `Invalid_op of char | `Unknown of string ]

  type t = Equations.t [@@deriving sexp_of]

  let of_string (s : string) : t =
    let monkeys =
      let open Angstrom in
      let name = take_while1 Char.is_alpha in
      let op = char '+' <|> char '-' <|> char '*' <|> char '/' in
      let binop =
        let open Angstrom.Let_syntax in
        let%bind l = name in
        let%bind op = char ' ' *> op <* char ' ' in
        let%map r = name in
        match op with
        | '+' -> Formula.(var l + var r)
        | '-' -> Formula.(var l - var r)
        | '*' -> Formula.(var l * var r)
        | '/' -> Formula.(var l / var r)
        | _ -> raise (Parse_error (`Invalid_op op))
      in
      let const =
        take_while1 Char.is_digit >>| fun d -> Formula.int (Int.of_string d)
      in
      let expr = binop <|> const in
      let monkey =
        name >>= fun n ->
        string ": " *> expr >>| fun e -> (n, e)
      in
      sep_by1 (char '\n') monkey
    in
    match Angstrom.parse_string ~consume:All monkeys s with
    | Ok v -> Equations.of_alist v
    | Error msg -> raise (Parse_error (`Unknown msg))

  let correct_and_solve (monkeys : t) : int =
    let open Formula in
    let root_eq =
      match arith_args (Equations.find_exn monkeys ~var:"root") with
      | [ e1; e2 ] -> equal e1 e2
      | _ -> raise Invalid_root
    in

    Equations.remove monkeys ~var:"root";
    Equations.remove monkeys ~var:"humn";
    (* NOTE: Try to pre-cache some results. *)
    List.iter (Equations.variables monkeys) ~f:(fun name ->
        let _ = Equations.get monkeys ~var:name in
        ());

    let map_eqs =
      Equations.to_alist monkeys
      |> List.map ~f:(fun (name, expr) -> equal (var name) expr)
      |> all
    in
    (map_eqs && root_eq) |> solve_for_variable ~var:"humn"
end

let part1 input =
  input |> Monkeys.of_string |> Equations.get ~var:"root" |> Option.value_exn
  |> Int.to_string

let%expect_test "part1" =
  let result = part1 example in
  print_endline result;
  [%expect {| 152 |}]

let part2 input =
  input |> Monkeys.of_string |> Monkeys.correct_and_solve |> Int.to_string

let%expect_test "part2" =
  let result = part2 example in
  print_endline result;
  [%expect {| 301 |}]
