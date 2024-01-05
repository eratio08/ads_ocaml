(lang dune 3.12)
(name ads_ocaml)
(generate_opam_files true)
(source
 (github username/reponame))
(authors "Eike Lurz")
(maintainers "Eike Lurz")
(license MIT)
(documentation https://url/to/documentation)
(package
 (name ads_ocaml)
 (synopsis "Algorithms & Data Structure in OCaml")
 (description "Implementation of Algorithms and Data Structures in OCaml")
 (depends ocaml dune utop ocamlformat (alcotest :with-test) ppx_deriving fmt base patdiff)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
