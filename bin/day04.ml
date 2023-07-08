open Base
open Stdio

let input = Advent.read_lines "data/day04.txt"

let parse_elf elf =
  match String.split ~on:'-' elf with
  | [ low; high ] -> (Int.of_string low, Int.of_string high)
  | _ -> assert false

let parse_line line =
  match String.split ~on:',' line with
  | [ first; second ] -> (parse_elf first, parse_elf second)
  | _ -> assert false

let range_fully_overlap ((l1, h1), (l2, h2)) =
  (l1 >= l2 && h1 <= h2) || (l2 >= l1 && h2 <= h1)

(* To calculate all cases that should overlap, we can do the inverse of those
   with no_overlap *)
let no_overlap ((l1, h1), (l2, h2)) = l2 > h1 || l1 > h2

(* This solves both part1 and part2 *)
let rec solve c1 c2 = function
  | [] | "" :: [] -> (c1, c2)
  | hd :: rest ->
      let only_full = if range_fully_overlap (parse_line hd) then 1 else 0 in
      let with_section = if no_overlap (parse_line hd) then 0 else 1 in
      solve (c1 + only_full) (c2 + with_section) rest

let () =
  let c1, c2 = solve 0 0 input in
  printf "%d\n%d\n" c1 c2
