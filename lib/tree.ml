module Tree (V : Ord.Ord) = struct
  type t =
    | Empty
    | Cons of t * V.t * t
end
