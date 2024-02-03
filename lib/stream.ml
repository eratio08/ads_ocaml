module type Stream = sig
  type 'a t

  val ( ++ ) : 'a t -> 'a t -> 'a t
  val take : int -> 'a t -> 'a t
  val drop : int -> 'a t -> 'a t
  val reverse : 'a t -> 'a t
end

module Stream = struct
  type 'a t =
    | Nil
    | Cons of ('a * 'a t) Stdlib.Lazy.t

  let rec ( ++ ) t1 t2 =
    match t1, t2 with
    | Nil, t2 -> t2
    | Cons (lazy (x, s)), t -> Cons (lazy (x, s ++ t))
  ;;

  let rec take n t =
    match n, t with
    | 0, _ -> Nil
    | _, Nil -> Nil
    | n, Cons (lazy (x, s)) -> Cons (lazy (x, take (n - 1) s))
  ;;

  let rec drop n t =
    match n, t with
    | 0, s -> s
    | _, Nil -> Nil
    | n, Cons (lazy (_, s)) -> drop (n - 1) s
  ;;

  let rec reverse t =
    let rec reverse' = function
      | Nil, r -> r
      | Cons (lazy (x, s)), r -> reverse' (s, Cons (lazy (x, r)))
    in
    reverse' (t, Nil)
  ;;
end

module StreamFn = struct
  type 'a t =
    | Empty
    | Cons of
        { head : unit -> 'a
        ; tail : unit -> 'a t
        }

  let rec ( ++ ) t1 t2 =
    match t1, t2 with
    | Empty, t -> t
    | Cons t1, t2 -> Cons { head = t1.head; tail = (fun () -> t1.tail () ++ t2) }
  ;;

  let rec take n t =
    match n, t with
    | 0, _ -> Empty
    | _, Empty -> Empty
    | n, Cons { head; tail } -> Cons { head; tail = (fun () -> take (n - 1) (tail ())) }
  ;;

  let rec drop n t =
    match n, t with
    | 0, t -> t
    | _, Empty -> Empty
    | n, Cons { tail; _ } -> drop (n - 1) (tail ())
  ;;

  let reverse t =
    let rec reverse' = function
      | Empty, t -> t
      | Cons t1, t2 ->
        reverse' (t1.tail (), Cons { head = t1.head; tail = (fun () -> t2) })
    in
    reverse' (t, Empty)
  ;;
end
