(* Models the native lazy *)
module MyLazy = struct
  type 'a lazy_state =
    | Delayed of (unit -> 'a)
    | Value of 'a
    | Exn of exn

  type 'a t = { mutable state : 'a lazy_state }

  let lazy' f = { state = Delayed f }

  let force l =
    match l.state with
    | Value x -> x
    | Exn e -> raise e
    | Delayed f ->
      (try
         let x = f () in
         l.state <- Value x;
         x
       with
       | exn ->
         l.state <- Exn exn;
         raise exn)
  ;;
end
