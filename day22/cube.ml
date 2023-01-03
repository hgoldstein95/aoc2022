open! Core
module Facing = Grid.Facing

type face = { label : int; direction : Facing.t } [@@deriving sexp]

type t = {
  front : face;
  back : face;
  left : face;
  right : face;
  top : face;
  bottom : face;
}
[@@deriving sexp]

let rotate_right (c : t) : t =
  {
    c with
    front = { c.front with direction = Facing.right c.front.direction };
    back = { c.back with direction = Facing.left c.back.direction };
  }

let rec reorient (c : t) : int * t =
  match c.front.direction with
  | Facing.North -> (0, c)
  | _ ->
      let i, c' = reorient (rotate_right c) in
      (i + 1, c')

let turn_down (c : t) : t =
  let { front; back; left; right; top; bottom } = c in
  {
    front = top;
    back = bottom;
    left = { left with direction = Facing.right left.direction };
    right = { right with direction = Facing.left left.direction };
    top = back;
    bottom = front;
  }

let turn_up (c : t) : t =
  let { front; back; left; right; top; bottom } = c in
  {
    front = bottom;
    back = top;
    left = { left with direction = Facing.left left.direction };
    right = { right with direction = Facing.right right.direction };
    top = front;
    bottom = back;
  }

let turn_left (c : t) : t =
  let { front; back; left; right; top; bottom } = c in
  {
    front = right;
    back = left;
    left = front;
    right = back;
    top = { top with direction = Facing.right top.direction };
    bottom = { bottom with direction = Facing.left bottom.direction };
  }

let turn_right (c : t) : t =
  let { front; back; left; right; top; bottom } = c in
  {
    front = left;
    back = right;
    left = back;
    right = front;
    top = { top with direction = Facing.left top.direction };
    bottom = { bottom with direction = Facing.right bottom.direction };
  }
