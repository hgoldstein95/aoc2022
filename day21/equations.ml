open! Core

exception Not_found

type t = int Formula.t Hashtbl.M(String).t [@@deriving sexp_of]

let find (m : t) ~(var : string) : int Formula.t option = Hashtbl.find m var

let find_exn (m : t) ~(var : string) : int Formula.t =
  match Hashtbl.find m var with None -> raise Not_found | Some f -> f

let rec get (m : t) ~(var : string) : int option =
  let open Option.Let_syntax in
  let%bind e = find m ~var in
  let%map v = Formula.eval e (fun var -> get m ~var) in
  (* NOTE: Cache the computed value by replacing the formula with a
     constant. *)
  Hashtbl.set m ~key:var ~data:(Formula.int v);
  v

let of_alist (l : (string * int Formula.t) list) : t =
  Hashtbl.of_alist_exn (module String) l

let to_alist (m : t) : (string * int Formula.t) list = Hashtbl.to_alist m
let remove (m : t) ~(var : string) : unit = Hashtbl.remove m var
let variables (m : t) : string list = Hashtbl.keys m
