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
  type t = [ `Cd of fname | `Ls | `File of int * fname | `Dir of fname ]
  [@@deriving equal, compare, sexp_of, quickcheck]

  val to_string : t -> string
  val of_string : string -> t
end = struct
  type t = [ `Cd of fname | `Ls | `File of int * fname | `Dir of fname ]
  [@@deriving equal, compare, sexp_of, quickcheck]

  let to_string = function
    | `Cd dir -> sprintf "$ cd %s" dir
    | `Ls -> "$ ls"
    | `File (size, name) -> sprintf "%d %s" size name
    | `Dir name -> sprintf "dir %s" name

  let of_string s =
    match String.split s ~on:' ' with
    | [ "$"; "cd"; dir ] -> `Cd dir
    | [ "$"; "ls" ] -> `Ls
    | [ "dir"; dirname ] -> `Dir dirname
    | [ size; name ] -> `File (Int.of_string size, name)
    | _ -> failwithf "Invalid line: %s" s ()

  let%expect_test "to_string" =
    print_s [%sexp (of_string "$ cd /" : t)];
    [%expect {| (Cd /) |}];
    print_s [%sexp (of_string "$ ls" : t)];
    [%expect {| Ls |}];
    print_s [%sexp (of_string "10394 foo.txt" : t)];
    [%expect {| (File (10394 foo.txt)) |}];
    print_s [%sexp (of_string "10394 xyz" : t)];
    [%expect {| (File (10394 xyz)) |}];
    print_s [%sexp (of_string "dir bar" : t)];
    [%expect {| (Dir bar) |}]

  let%test_unit "roundtrip" =
    Quickcheck.test [%quickcheck.generator: t] ~sexp_of:[%sexp_of: t]
      ~shrinker:[%quickcheck.shrinker: t] ~f:(fun line ->
        [%test_eq: t] (of_string (to_string line)) line)
end

let%test_unit "TermLine.of_string" =
  example |> String.split_lines
  |> List.map ~f:TermLine.of_string
  |> List.map ~f:TermLine.to_string
  |> String.concat ~sep:"\n"
  |> [%test_eq: string] example

module FileSystem = struct
  type fobj = [ `File of int * fname | `Dir of fname ]
  [@@deriving equal, compare, sexp_of]

  type t = (fname * fobj list) list [@@deriving equal, compare, sexp_of]

  let of_term_lines (lines : TermLine.t list) : t =
    let mk_fname wd = wd |> List.rev |> String.concat ~sep:"/" |> ( ^ ) "/" in
    List.fold_left lines ~init:([], []) ~f:(fun (working_dir, fs) line ->
        match line with
        | `Cd "/" -> ([], fs)
        | `Cd ".." -> (List.tl_exn working_dir, fs)
        | `Cd dir -> (dir :: working_dir, fs)
        | `Ls -> (working_dir, fs)
        | #fobj as obj -> (working_dir, (mk_fname working_dir, obj) :: fs))
    |> snd
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
    |> List.group ~break:(fun (a, _) (b, _) -> not (String.equal a b))
    |> List.map ~f:(fun xs -> (fst (List.hd_exn xs), List.map ~f:snd xs))

  let of_string s =
    s |> String.split_lines |> List.map ~f:TermLine.of_string |> of_term_lines

  let dir_sizes fs =
    fs
    |> List.map ~f:(fun (dir, _) ->
           ( dir,
             fs
             |> List.map ~f:(fun (d, f) ->
                    if String.is_prefix ~prefix:dir d then
                      List.map f ~f:(function
                        | `File (size, _) -> size
                        | `Dir _ -> 0)
                      |> List.sum (module Int) ~f:Fn.id
                    else 0)
             |> List.sum (module Int) ~f:Fn.id ))
end

let%expect_test "FileSystem.of_string" =
  print_s [%sexp (FileSystem.of_string example : FileSystem.t)];
  [%expect
    {|
    ((/ ((Dir d) (File (8504156 c.dat)) (File (14848514 b.txt)) (Dir a)))
     (/a ((File (62596 h.lst)) (File (2557 g)) (File (29116 f)) (Dir e)))
     (/a/e ((File (584 i))))
     (/d
      ((File (7214296 k)) (File (5626152 d.ext)) (File (8033020 d.log))
       (File (4060174 j))))) |}]

let%expect_test "FileSystem.dir_sizes" =
  print_s
    [%sexp
      (FileSystem.dir_sizes (FileSystem.of_string example) : (fname * int) list)];
  [%expect {| ((/ 48381165) (/a 94853) (/a/e 584) (/d 24933642)) |}]

let part1 input =
  input |> FileSystem.of_string |> FileSystem.dir_sizes |> List.map ~f:snd
  |> List.filter ~f:(fun x -> x < 100_000)
  |> List.sum (module Int) ~f:Fn.id
  |> Int.to_string

let%expect_test _ =
  let result = part1 example in
  print_endline result;
  [%expect {| 95437 |}]

let min_to_recover dir_sizes =
  let total_space = 70_000_000 in
  let unused_need = 30_000_000 in
  let current_used = List.Assoc.find_exn ~equal:String.equal dir_sizes "/" in
  unused_need - (total_space - current_used)

let%expect_test _ =
  let result =
    min_to_recover (example |> FileSystem.of_string |> FileSystem.dir_sizes)
    |> Int.to_string
  in
  print_endline result;
  [%expect {| 8381165 |}]

let part2 input =
  let dir_sizes = input |> FileSystem.of_string |> FileSystem.dir_sizes in
  let mtr = min_to_recover dir_sizes in
  dir_sizes
  |> List.filter ~f:(fun (_, size) -> size > mtr)
  |> List.min_elt ~compare:(fun (_, a) (_, b) -> Int.compare a b)
  |> Option.value_exn |> snd |> Int.to_string

let%expect_test _ =
  let result = part2 example in
  print_endline result;
  [%expect {| 24933642 |}]
