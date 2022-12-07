open! Core
open! Stdio
module G = Core.Quickcheck.Generator

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

type fname = (string[@quickcheck.generator String.gen_nonempty' Char.gen_alpha])
[@@deriving equal, compare, sexp_of, quickcheck]

module TermLine : sig
  type t = Cd of string | Ls | File of int * string | Dir of string
  [@@deriving equal, compare, sexp_of, quickcheck]

  val to_string : t -> string
  val of_string : string -> t
end = struct
  type t = Cd of fname | Ls | File of int * fname | Dir of fname
  [@@deriving equal, compare, sexp_of, quickcheck]

  let to_string = function
    | Cd dir -> sprintf "$ cd %s" dir
    | Ls -> "$ ls"
    | File (size, name) -> sprintf "%d %s" size name
    | Dir name -> sprintf "dir %s" name

  let of_string s =
    match String.split s ~on:' ' with
    | [ "$"; "cd"; dir ] -> Cd dir
    | [ "$"; "ls" ] -> Ls
    | [ "dir"; dirname ] -> Dir dirname
    | [ size; name ] -> File (Int.of_string size, name)
    | _ -> failwithf "Invalid line: %s" s ()

  let%expect_test "to_string" =
    print_s [%sexp (of_string "$ cd /" : t)];
    [%expect {| (Cd /) |}];
    print_s [%sexp (of_string "$ ls" : t)];
    [%expect {| Ls |}];
    print_s [%sexp (of_string "10394 foo.txt" : t)];
    [%expect {| (File 10394 foo.txt) |}];
    print_s [%sexp (of_string "10394 xyz" : t)];
    [%expect {| (File 10394 xyz) |}];
    print_s [%sexp (of_string "dir bar" : t)];
    [%expect {| (Dir bar) |}]

  let%test_unit "roundtrip" =
    Quickcheck.test [%quickcheck.generator: t] ~sexp_of:[%sexp_of: t]
      ~shrinker:[%quickcheck.shrinker: t] ~f:(fun line ->
        [%test_eq: t] (of_string (to_string line)) line)
end

let%test_unit "TermLine roundtrip" =
  example |> String.split_lines
  |> List.map ~f:TermLine.of_string
  |> List.map ~f:TermLine.to_string
  |> String.concat ~sep:"\n"
  |> [%test_eq: string] example

module FileSystem = struct
  type t = File of fname | Directory of (fname * t) list
  [@@deriving equal, compare, sexp_of, variants]

  let quickcheck_generator =
    let open G.Let_syntax in
    let g_fname = G.map ~f:file [%quickcheck.generator: fname] in
    let rec aux n =
      if n = 0 then g_fname
      else
        G.weighted_union
          [
            (1., g_fname);
            ( 5.,
              G.map ~f:directory
                (G.list_non_empty
                   (let%bind f = [%quickcheck.generator: fname] in
                    let%map x = aux (n - 1) in
                    (f, x))) );
          ]
    in
    let%map x = aux 9 in
    ("/", [ x ])
end

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
