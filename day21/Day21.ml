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
  exception Parse_error

  type expr =
    | Add of expr * expr
    | Sub of expr * expr
    | Mul of expr * expr
    | Div of expr * expr
    | Const of int
    | Var of string
  [@@deriving sexp]

  type t = [ `Cached of int | `Expr of expr ] Hashtbl.M(String).t
  [@@deriving sexp]

  let of_string (s : string) : t =
    let open Angstrom in
    let name = take_while1 Char.is_alpha in
    let op = char '+' <|> char '-' <|> char '*' <|> char '/' in
    let binop =
      name >>= fun l ->
      char ' ' *> op >>= fun op ->
      char ' ' *> name >>| fun r ->
      match op with
      | '+' -> Add (Var l, Var r)
      | '-' -> Sub (Var l, Var r)
      | '*' -> Mul (Var l, Var r)
      | '/' -> Div (Var l, Var r)
      | _ -> raise Parse_error
    in
    let const =
      take_while1 Char.is_digit >>| fun d -> Const (Int.of_string d)
    in
    let expr = binop <|> const in
    let key_value =
      name >>= fun n ->
      string ": " *> expr >>| fun e -> (n, `Expr e)
    in
    let parse_line line =
      match Angstrom.parse_string ~consume:All key_value line with
      | Ok blueprint -> blueprint
      | Error msg -> failwith msg
    in
    s |> String.split_lines |> List.map ~f:parse_line
    |> Hashtbl.of_alist_exn (module String)

  let rec get (monkeys : t) ~(name : string) =
    match Hashtbl.find monkeys name with
    | None -> None
    | Some (`Cached v) -> Some v
    | Some (`Expr e) ->
        let open Option.Let_syntax in
        let%map v = eval monkeys e in
        Hashtbl.set monkeys ~key:name ~data:(`Cached v);
        v

  and eval (monkeys : t) (expr : expr) : int option =
    let open Option.Let_syntax in
    match expr with
    | Add (e1, e2) ->
        let%bind v1 = eval monkeys e1 in
        let%map v2 = eval monkeys e2 in
        v1 + v2
    | Sub (e1, e2) ->
        let%bind v1 = eval monkeys e1 in
        let%map v2 = eval monkeys e2 in
        v1 - v2
    | Mul (e1, e2) ->
        let%bind v1 = eval monkeys e1 in
        let%map v2 = eval monkeys e2 in
        v1 * v2
    | Div (e1, e2) ->
        let%bind v1 = eval monkeys e1 in
        let%map v2 = eval monkeys e2 in
        v1 / v2
    | Const i -> Some i
    | Var n -> get monkeys ~name:n

  let to_formula (monkeys : t) : bool Formula.t =
    let open Formula in
    let rec expr_to_formula = function
      | Add (e1, e2) -> expr_to_formula e1 + expr_to_formula e2
      | Sub (e1, e2) -> expr_to_formula e1 - expr_to_formula e2
      | Mul (e1, e2) -> expr_to_formula e1 * expr_to_formula e2
      | Div (e1, e2) -> expr_to_formula e1 / expr_to_formula e2
      | Const i -> int i
      | Var s -> var s
    in
    let root =
      match Hashtbl.find monkeys "root" with
      | None -> raise Monkey_not_found
      | Some v -> (
          match v with
          | `Expr (Add (e1, e2))
          | `Expr (Sub (e1, e2))
          | `Expr (Mul (e1, e2))
          | `Expr (Div (e1, e2)) ->
              equal (expr_to_formula e1) (expr_to_formula e2)
          | _ -> raise Invalid_root)
    in

    Hashtbl.remove monkeys "root";
    Hashtbl.remove monkeys "humn";
    (* NOTE: We do this so we can pre-evaluate a bunch of values. *)
    List.iter (Hashtbl.keys monkeys) ~f:(fun name ->
        let _ = get monkeys ~name in
        ());

    monkeys |> Hashtbl.to_alist
    |> List.map ~f:(fun (name, expr) ->
           match expr with
           | `Cached v -> equal (var name) (int v)
           | `Expr e -> equal (var name) (expr_to_formula e))
    |> all
    && root
end

let part1 input =
  input |> Monkeys.of_string |> Monkeys.get ~name:"root" |> Option.value_exn
  |> Int.to_string

let%expect_test "part1" =
  let result = part1 example in
  print_endline result;
  [%expect {| 152 |}]

let part2 input =
  input |> Monkeys.of_string |> Monkeys.to_formula
  |> Formula.solve_for_variable ~var:"humn"
  |> Int.to_string

let%expect_test "part2" =
  let result = part2 example in
  print_endline result;
  [%expect {| 301 |}]
