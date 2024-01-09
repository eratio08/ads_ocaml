(*
   Empty nodes are Black

   Invariant 1. No red node has a red child.
   Invariant 2. Every path from the root to an empty node contains the same
   number of black nodes.
*)
module RedBalckTree (Elem : Ord.Ord) = struct
  type e = Elem.t

  type color =
    | Red
    | Black

  type t =
    | Empty
    | Tree of color * t * e * t

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false
  ;;

  let rec member x = function
    | Empty -> false
    | Tree (_, l, v, _) when x < v -> member x l
    | Tree (_, _, v, r) when x > v -> member x r
    | Tree (_, _, _, _) -> true
  ;;

  (* Turn Black-Red-Red into Red-Black-Black *)
  let balance = function
    | Black, Tree (Red, Tree (Red, a, x, b), y, c), z, d
    | Black, Tree (Red, a, x, Tree (Red, b, y, c)), z, d
    | Black, a, x, Tree (Red, Tree (Red, b, y, c), z, d)
    | Black, a, x, Tree (Red, b, y, Tree (Red, c, z, d)) ->
      Tree (Red, Tree (Black, a, x, b), y, Tree (Black, c, z, d))
    | color, a, x, b -> Tree (color, a, x, b)
  ;;

  let insert x t =
    let rec ins = function
      (* New trees are Red *)
      | Empty -> Tree (Red, Empty, x, Empty)
      | Tree (color, a, y, b) when x < y -> balance (color, ins a, y, b)
      | Tree (color, a, y, b) when x > y -> balance (color, a, y, ins b)
      | t -> t
    in
    match ins t with
    | Empty -> failwith "empty"
    (* Final root is forced Black *)
    | Tree (_, a, y, b) -> Tree (Black, a, y, b)
  ;;
end
