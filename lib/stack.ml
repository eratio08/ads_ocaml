module type Mutable_stack = sig
  type 'a t

  val make : 'a -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> unit
  val top : 'a t -> 'a
  val height : 'a t -> int
end

module type Stack = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val cons : 'a -> 'a t -> 'a t
  val head : 'a t -> 'a option
  val head_exn : 'a t -> 'a
  val tail : 'a t -> 'a t option
  val tail_exn : 'a t -> 'a t
end

module List : Stack = struct
  type 'a t = 'a list

  let empty = []

  let is_empty = function
    | [] -> true
    | _ -> false
  ;;

  let cons a t = a :: t

  let head = function
    | [] -> None
    | x :: _ -> Some x
  ;;

  let head_exn = function
    | [] -> failwith "list is empty"
    | x :: _ -> x
  ;;

  let tail = function
    | [] -> None
    | _ :: t -> Some t
  ;;

  let tail_exn = function
    | [] -> failwith "list is empty"
    | _ :: t -> t
  ;;
end

module AdtStack : Stack = struct
  type 'a t =
    | Empty
    | Cons of 'a * 'a t

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false
  ;;

  let cons x t = Cons (x, t)

  let head = function
    | Empty -> None
    | Cons (x, _) -> Some x
  ;;

  let head_exn = function
    | Empty -> failwith "stack is empty"
    | Cons (x, _) -> x
  ;;

  let tail = function
    | Empty -> None
    | Cons (_, t) -> Some t
  ;;

  let tail_exn = function
    | Empty -> failwith "stack is empty"
    | Cons (_, t) -> t
  ;;

  let rec append t1 t2 =
    match t1, t2 with
    | Empty, ys -> ys
    | Cons (x, xs), ys -> Cons (x, append xs ys)
  ;;

  let rec update_exn t i x =
    match t, i, x with
    | Empty, _, _ -> failwith "is empty"
    | Cons (_, xs), 0, y -> Cons (y, xs)
    | Cons (x, xs), n, y -> Cons (x, update_exn xs (n - 1) y)
  ;;

  (* val suffix : 'a t -> 'a t t*)
  let rec suffix = function
    | Empty -> Empty
    | Cons (_, xs) as t -> Cons (t, suffix xs)
  ;;
end
