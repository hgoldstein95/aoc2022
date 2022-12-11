open! Core
open! Stdio

let example = In_channel.read_all "example.elfs"

module Instr : sig
  type t = Noop | Addx of int [@@deriving sexp_of, equal]

  val of_string : string -> t
end = struct
  type t = Noop | Addx of int [@@deriving sexp_of, equal]

  let of_string s =
    match String.split_on_chars ~on:[ ' ' ] s with
    | [ "noop" ] -> Noop
    | [ "addx"; i ] -> Addx (Int.of_string i)
    | _ -> failwith "invalid instruction"
end

module CPU : sig
  val run : Instr.t list -> int list
end = struct
  type mem = int
  type t = { rev_trace : mem list; reg : mem } [@@deriving fields]

  let step cpu instr =
    match instr with
    | Instr.Noop -> { cpu with rev_trace = cpu.reg :: cpu.rev_trace }
    | Instr.Addx x ->
        let reg = cpu.reg + x in
        { reg; rev_trace = cpu.reg :: cpu.reg :: cpu.rev_trace }

  let run instrs =
    List.fold instrs ~init:{ rev_trace = []; reg = 1 } ~f:step
    |> rev_trace |> List.rev
end

let part1 input =
  let special = [ 20; 60; 100; 140; 180; 220 ] in
  input |> String.split_lines
  |> List.map ~f:Instr.of_string
  |> CPU.run
  |> List.mapi ~f:(fun i x ->
         let i = i + 1 in
         if List.mem special ~equal:( = ) i then x * i else 0)
  |> List.sum (module Int) ~f:Fn.id
  |> Int.to_string

let%expect_test "part1" =
  let result = part1 example in
  print_endline result;
  [%expect {| 13140 |}]

let part2 input =
  print_endline "";
  let trace =
    input |> String.split_lines |> List.map ~f:Instr.of_string |> CPU.run
  in
  List.init (List.length trace) ~f:(fun i -> i % 40)
  |> List.zip_exn trace
  |> List.map ~f:(fun (r_val, sprite_middle) ->
         if abs (r_val - sprite_middle) < 2 then '#' else '.')
  |> List.chunks_of ~length:40
  |> List.map ~f:String.of_char_list
  |> String.concat ~sep:"\n"

let%expect_test "part2" =
  let result = part2 example in
  print_endline result;
  [%expect
    {|
    ##..##..##..##..##..##..##..##..##..##..
    ###...###...###...###...###...###...###.
    ####....####....####....####....####....
    #####.....#####.....#####.....#####.....
    ######......######......######......####
    #######.......#######.......#######..... |}]
