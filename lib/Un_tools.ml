open Base
module G = Base_quickcheck.Generator

let un_max v =
  let open G.Let_syntax in
  let%bind xs = G.list (G.int_inclusive 1 (v - 1)) in
  G.list_permutations (v :: xs)

let un_sum n =
  let open G.Let_syntax in
  let rec aux (n, sz) =
    if n = 0 then return []
    else if sz = 0 then return [ n ]
    else if n = 1 then return [ 1 ]
    else
      let%bind x = G.int_inclusive 1 (n - 1) in
      let%map xs = aux (n - x, sz - 1) in
      x :: xs
  in
  aux (n, 5) >>= G.list_permutations

module List = struct
  let sequence xs =
    let open G.Let_syntax in
    List.fold_left
      ~f:(fun acc_m x_m ->
        let%bind x = x_m in
        let%map acc = acc_m in
        x :: acc)
      ~init:(return []) xs

  let map_m xs ~f = xs |> List.map ~f |> sequence
end
