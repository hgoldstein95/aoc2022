open! Core
open! Stdio

let example =
  "Monkey 0:\n\
  \  Starting items: 79, 98\n\
  \  Operation: new = old * 19\n\
  \  Test: divisible by 23\n\
  \    If true: throw to monkey 2\n\
  \    If false: throw to monkey 3\n\n\
   Monkey 1:\n\
  \  Starting items: 54, 65, 75, 74\n\
  \  Operation: new = old + 6\n\
  \  Test: divisible by 19\n\
  \    If true: throw to monkey 2\n\
  \    If false: throw to monkey 0\n\n\
   Monkey 2:\n\
  \  Starting items: 79, 60, 97\n\
  \  Operation: new = old * old\n\
  \  Test: divisible by 13\n\
  \    If true: throw to monkey 1\n\
  \    If false: throw to monkey 3\n\n\
   Monkey 3:\n\
  \  Starting items: 74\n\
  \  Operation: new = old + 3\n\
  \  Test: divisible by 17\n\
  \    If true: throw to monkey 0\n\
  \    If false: throw to monkey 1"

module Expr = struct
  type leaf = Old | Const of int [@@deriving sexp_of]
  type t = Add of (leaf * leaf) | Mul of (leaf * leaf) [@@deriving sexp_of]

  let eval old =
    let eval_leaf = function Old -> old | Const i -> i in
    function
    | Add (e1, e2) -> eval_leaf e1 + eval_leaf e2
    | Mul (e1, e2) -> eval_leaf e1 * eval_leaf e2
end

module Monkey = struct
  type message = { target : int; worry : int } [@@deriving sexp_of]

  type t = {
    id : int;
    operation : Expr.t;
    test : int;
    if_true : int;
    if_false : int;
    mutable items : int list;
    mutable inspections : int;
  }
  [@@deriving sexp_of, fields]

  let expr_of_string s =
    let arg_of_string = function
      | "old" -> Expr.Old
      | s -> Expr.Const (Int.of_string s)
    in
    match String.split s ~on:' ' with
    | [ e1; "+"; e2 ] -> Expr.Add (arg_of_string e1, arg_of_string e2)
    | [ e1; "*"; e2 ] -> Expr.Mul (arg_of_string e1, arg_of_string e2)
    | _ -> failwith "invalid expr"

  let of_string s =
    let matches =
      let open Re in
      Posix.compile_pat
        "Monkey ([0-9]+):\n\
        \  Starting items: (.*)\n\
        \  Operation: new = (.*)\n\
        \  Test: divisible by ([0-9]+)\n\
        \    If true: throw to monkey ([0-9]+)\n\
        \    If false: throw to monkey ([0-9]+)"
      |> Fn.flip exec s |> Group.all
    in
    match matches with
    | [| _; id; starting_items; operation; test; if_true; if_false |] ->
        {
          id = Int.of_string id;
          operation = expr_of_string operation;
          test = Int.of_string test;
          if_true = Int.of_string if_true;
          if_false = Int.of_string if_false;
          items =
            String.split starting_items ~on:','
            |> List.map ~f:(fun s -> s |> String.strip |> Int.of_string);
          inspections = 0;
        }
    | _ -> failwith "invalid input"

  let take_turn ~capped ~modulus m =
    m.items
    |> List.map ~f:(fun old ->
           let new_ =
             Expr.eval old m.operation mod modulus |> fun x ->
             if capped then x / 3 else x
           in
           let target = if new_ mod m.test = 0 then m.if_true else m.if_false in
           { target; worry = new_ })

  let%expect_test "of_string and take_turn" =
    let monkey =
      of_string
        "Monkey 0:\n\
        \  Starting items: 79, 98\n\
        \  Operation: new = old * 19\n\
        \  Test: divisible by 23\n\
        \    If true: throw to monkey 2\n\
        \    If false: throw to monkey 3"
    in
    print_s [%sexp (monkey : t)];
    [%expect
      {|
      ((id 0) (operation (Mul (Old (Const 19)))) (test 23) (if_true 2) (if_false 3)
       (items (79 98)) (inspections 0)) |}];
    print_s
      [%sexp (take_turn ~capped:true ~modulus:1000000 monkey : message list)];
    [%expect {| (((target 3) (worry 500)) ((target 3) (worry 620))) |}]
end

module Monkeys = struct
  type t = { monkeys : Monkey.t array } [@@deriving sexp_of]

  let of_string s =
    {
      monkeys =
        s |> String.split_lines
        |> List.group ~break:(fun _ s -> String.is_empty s)
        |> List.map ~f:(List.drop_while ~f:String.is_empty)
        |> List.map ~f:(fun s ->
               s |> String.concat ~sep:"\n" |> Monkey.of_string)
        |> Array.of_list;
    }

  let round ?(capped = true) m =
    let modulus = Array.fold m.monkeys ~init:1 ~f:(fun acc m -> acc * m.test) in
    for i = 0 to Array.length m.monkeys - 1 do
      let items = List.length m.monkeys.(i).items in
      let messages = Monkey.take_turn ~capped ~modulus m.monkeys.(i) in

      m.monkeys.(i).inspections <- m.monkeys.(i).inspections + items;
      m.monkeys.(i).items <- [];
      List.iter messages ~f:(fun { target; worry } ->
          m.monkeys.(target).items <- m.monkeys.(target).items @ [ worry ])
    done
end

let%expect_test "Monkeys.of_string and Monkeys.round" =
  let ms = Monkeys.of_string example in
  [%sexp (ms : Monkeys.t)] |> print_s;
  [%expect
    {|
    ((monkeys
      (((id 0) (operation (Mul (Old (Const 19)))) (test 23) (if_true 2)
        (if_false 3) (items (79 98)) (inspections 0))
       ((id 1) (operation (Add (Old (Const 6)))) (test 19) (if_true 2)
        (if_false 0) (items (54 65 75 74)) (inspections 0))
       ((id 2) (operation (Mul (Old Old))) (test 13) (if_true 1) (if_false 3)
        (items (79 60 97)) (inspections 0))
       ((id 3) (operation (Add (Old (Const 3)))) (test 17) (if_true 0)
        (if_false 1) (items (74)) (inspections 0))))) |}];
  Monkeys.round ms;
  [%sexp (ms.monkeys |> Array.map ~f:Monkey.items : int list array)] |> print_s;
  [%expect {| ((20 23 27 26) (2080 25 167 207 401 1046) () ()) |}];
  for _ = 0 to 18 do
    Monkeys.round ms
  done;
  [%sexp (ms.monkeys |> Array.map ~f:Monkey.inspections : int array)] |> print_s;
  [%expect {| (101 95 7 105) |}]

let part1 input =
  let ms = Monkeys.of_string input in
  for _ = 0 to 19 do
    Monkeys.round ms
  done;
  Array.sort
    ~compare:(fun m1 m2 -> Int.compare m2.inspections m1.inspections)
    ms.monkeys;
  ms.monkeys.(0).inspections * ms.monkeys.(1).inspections |> Int.to_string

let%expect_test "part1" =
  let result = part1 example in
  print_endline result;
  [%expect {| 10605 |}]

let part2 input =
  let ms = Monkeys.of_string input in
  for _ = 0 to 9999 do
    Monkeys.round ~capped:false ms
  done;
  Array.sort
    ~compare:(fun m1 m2 -> Int.compare m2.inspections m1.inspections)
    ms.monkeys;
  ms.monkeys.(0).inspections * ms.monkeys.(1).inspections |> Int.to_string

let%expect_test "part2" =
  let result = part2 example in
  print_endline result;
  [%expect {| 2713310158 |}]
