module Tree (V : Ord.Ord) = struct
  type e = V.t

  type t =
    | Empty
    | Cons of t * e * t
end

(* TODO implement conversion from splay heap to bin tree *)
module BinTree (Elem : Ord.Ord) = struct
  type e = Elem.t

  type t =
    | E
    | T of e * t * t
end
