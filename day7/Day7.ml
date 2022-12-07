open! Core
open! Stdio

let example =
  "$ cd /\n\
   $ ls\n\
   dir a\n\
   14848514 b.txt\n\
   8504156 c.dat\n\
   dir d\n\
   $ cd a\n\
   $ ls\n\
   dir e\n\
   29116 f\n\
   2557 g\n\
   62596 h.lst\n\
   $ cd e\n\
   $ ls\n\
   584 i\n\
   $ cd ..\n\
   $ cd ..\n\
   $ cd d\n\
   $ ls\n\
   4060174 j\n\
   8033020 d.log\n\
   5626152 d.ext\n\
   7214296 k"

type fs_line = Cd of string | Ls | File of int * string | Dir of string
[@@deriving equal, compare, sexp_of, quickcheck]

let fs_line_to_string = function
  | Cd dir -> sprintf "$ cd %s" dir
  | Ls -> "$ ls"
  | File (size, name) -> sprintf "%d %s" size name
  | Dir name -> sprintf "dir %s" name

let parse_line s =
  match String.split s ~on:' ' with
  | [ "$"; "cd"; dir ] -> Cd dir
  | [ "$"; "ls" ] -> Ls
  | [ "dir"; dirname ] -> Dir dirname
  | [ size; name ] -> File (Int.of_string size, name)
  | _ -> failwithf "Invalid line: %s" s ()

let%expect_test "parse_line" =
  print_s [%sexp (parse_line "$ cd /" : fs_line)];
  [%expect {| (Cd /) |}];
  print_s [%sexp (parse_line "$ ls" : fs_line)];
  [%expect {| Ls |}];
  print_s [%sexp (parse_line "10394 foo.txt" : fs_line)];
  [%expect {| (File 10394 foo.txt) |}];
  print_s [%sexp (parse_line "10394 xyz" : fs_line)];
  [%expect {| (File 10394 xyz) |}];
  print_s [%sexp (parse_line "dir bar" : fs_line)];
  [%expect {| (Dir bar) |}]

let%test_unit "parse_line roundtrip" =
  example |> String.split_lines |> List.map ~f:parse_line
  |> List.map ~f:fs_line_to_string
  |> String.concat ~sep:"\n"
  |> [%test_eq: string] example

(** TODO Fix generator *)
let%test_unit "parse_line roundtrip qc" =
  Quickcheck.test [%quickcheck.generator: fs_line] ~sexp_of:[%sexp_of: fs_line]
    ~f:(fun line ->
      [%test_eq: fs_line] (parse_line (fs_line_to_string line)) line)

let part1 _ = ""

let%expect_test _ =
  let result = part1 example in
  print_endline result;
  [%expect {|  |}]

let part2 _ = ""

let%expect_test _ =
  let result = part2 example in
  print_endline result;
  [%expect {|  |}]
