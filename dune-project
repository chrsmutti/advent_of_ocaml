(lang dune 3.9)

(name advent_of_ocaml)

(generate_opam_files true)

(source
 (github chrsmutti/advent_of_code))

(authors "Christian Mutti")

(license MIT)

(package
 (name advent_of_ocaml)
 (synopsis "Advent of OCaml - Solutions to Advent of Code puzzles in OCaml")
 (description
  "Advent of OCaml is a collection of personal solutions to the Advent of Code
  puzzles implemented in the OCaml programming language.")
 (depends ocaml dune)
 (tags
  (topics "Advent of Code" "OCaml programming" "Algorithmic problem-solving")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune
