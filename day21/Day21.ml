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
  exception Monkey_not_found
  exception Invalid_root
  exception Parse_error of [ `Invalid_op of char | `Unknown of string ]

  type t = int Formula.t Hashtbl.M(String).t

  let of_string (s : string) : t =
    let open Angstrom in
    let open Formula in
    let name = take_while1 Char.is_alpha in
    let op = char '+' <|> char '-' <|> char '*' <|> char '/' in
    let binop =
      let open Angstrom.Let_syntax in
      let%bind l = name in
      let%bind op = char ' ' *> op <* char ' ' in
      let%map r = name in
      match op with
      | '+' -> var l + var r
      | '-' -> var l - var r
      | '*' -> var l * var r
      | '/' -> var l / var r
      | _ -> raise (Parse_error (`Invalid_op op))
    in
    let const = take_while1 Char.is_digit >>| fun d -> int (Int.of_string d) in
    let expr = binop <|> const in
    let key_value =
      name >>= fun n ->
      string ": " *> expr >>| fun e -> (n, e)
    in
    let parse_line line =
      match Angstrom.parse_string ~consume:All key_value line with
      | Ok blueprint -> blueprint
      | Error msg -> raise (Parse_error (`Unknown msg))
    in
    s |> String.split_lines |> List.map ~f:parse_line
    |> Hashtbl.of_alist_exn (module String)

  let find (monkeys : t) ~(name : string) : int Formula.t option =
    Hashtbl.find monkeys name

  let find_exn (monkeys : t) ~(name : string) : int Formula.t =
    match Hashtbl.find monkeys name with
    | None -> raise Monkey_not_found
    | Some f -> f

  let rec get (monkeys : t) ~(name : string) : int option =
    let open Option.Let_syntax in
    let%bind e = find monkeys ~name in
    let%map v = Formula.eval e (fun name -> get monkeys ~name) in
    (* NOTE: Cache the computed value by replacing the formula with a
       constant. *)
    Hashtbl.set monkeys ~key:name ~data:(Formula.int v);
    v

  let correct_and_solve (monkeys : t) : int =
    let open Formula in
    let root_eq =
      match arith_args (find_exn monkeys ~name:"root") with
      | [ e1; e2 ] -> equal e1 e2
      | _ -> raise Invalid_root
    in

    Hashtbl.remove monkeys "root";
    Hashtbl.remove monkeys "humn";
    (* NOTE: Try to pre-cache some results. *)
    List.iter (Hashtbl.keys monkeys) ~f:(fun name ->
        let _ = get monkeys ~name in
        ());

    let map_eqs =
      Hashtbl.to_alist monkeys
      |> List.map ~f:(fun (name, expr) -> equal (var name) expr)
      |> all
    in
    (map_eqs && root_eq) |> solve_for_variable ~var:"humn"
end

let part1 input =
  input |> Monkeys.of_string |> Monkeys.get ~name:"root" |> Option.value_exn
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
