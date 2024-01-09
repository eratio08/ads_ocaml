open Ads_ocaml.Rb_tree

module StringRedBlackTree = struct
  module OrdString = struct
    include Ads_ocaml.Ord.Ord (String)

    let pp fmt t = Format.fprintf fmt "%s" t
  end

  include RedBalckTree (OrdString)

  let pp_color fmt = function
    | Red -> Format.fprintf fmt "Red"
    | Black -> Format.fprintf fmt "Black"
  ;;

  let rec pp fmt = function
    | Empty -> Format.fprintf fmt "empty"
    | Tree (color, l, v, r) ->
      Format.fprintf
        fmt
        "Tree (%a, %a, \"%a\", %a)"
        pp_color
        color
        (fun fmt -> pp fmt)
        l
        OrdString.pp
        v
        (fun fmt -> pp fmt)
        r
  ;;
end

let rb_tree = Alcotest.testable StringRedBlackTree.pp ( = )

let test_member_rb_tree () =
  let open StringRedBlackTree in
  Alcotest.(check bool) "member of empty" false (member "Member" empty);
  Alcotest.(check bool)
    "member of shallow that contains"
    true
    (member "Member" (Tree (Black, empty, "Member", empty)));
  Alcotest.(check bool)
    "member of shallow that does not contain"
    false
    (member "Not Member" (Tree (Black, empty, "Member", empty)));
  Alcotest.(check bool)
    "member of deep that contains"
    true
    (member
       "1"
       (Tree
          ( Black
          , Tree
              ( Red
              , Tree (Black, Tree (Red, empty, "0", empty), "1", empty)
              , "2"
              , Tree (Black, empty, "3", empty) )
          , "4"
          , Tree (Black, empty, "5", empty) )))
;;

let test_insert_rb_tree () =
  let open StringRedBlackTree in
  Alcotest.(check rb_tree)
    "insert in empty"
    (Tree (Black, empty, "First", empty))
    (insert "First" empty);
  Alcotest.(check rb_tree)
    "insert in non-empty"
    (Tree
       ( Black
       , Tree
           ( Red
           , Tree (Black, Tree (Red, empty, "0", empty), "1", empty)
           , "2"
           , Tree (Black, empty, "3", empty) )
       , "4"
       , Tree (Black, empty, "5", empty) ))
    (empty
     |> insert "5"
     |> insert "4"
     |> insert "3"
     |> insert "2"
     |> insert "1"
     |> insert "0")
;;

let suite =
  [ "RedBalckTree.member", `Quick, test_member_rb_tree
  ; "RedBalckTree.insert", `Quick, test_insert_rb_tree
  ]
;;
