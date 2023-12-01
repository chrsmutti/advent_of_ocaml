open Base
open Stdio

let input = Advent.read_lines "data/2022/day03.txt"

let priority char =
  let a_lower = Char.to_int 'a' in
  let z_lower = Char.to_int 'z' in
  let a_upper = Char.to_int 'A' in
  let z_upper = Char.to_int 'Z' in
  match Char.to_int char with
  | x when x >= a_lower && x <= z_lower -> x - a_lower + 1
  | x when x >= a_upper && x <= z_upper -> x - a_upper + 27
  | _ -> assert false

module Rucksack = struct
  type t = { chars : char list }

  let of_string s =
    let chars = String.to_list s in
    { chars }

  let inter r =
    let mid = List.length r.chars / 2 in
    let c1 = List.sub r.chars ~pos:0 ~len:mid |> Set.of_list (module Char) in
    let c2 = List.sub r.chars ~pos:mid ~len:mid |> Set.of_list (module Char) in
    Set.inter c1 c2 |> Set.to_list

  (* When rucksacks items are ordered by priority, we can iter through them by
     moving pointers until we find a position where all 3 rucksacks have the
     same prirority. I don't think the code below is all that readable, maybe
     I can optmise the readability in the future. It was really easy to write
     it though :D *)
  let badge r1 r2 r3 =
    let r1 = List.map r1.chars ~f:priority |> List.sort ~compare:Poly.compare in
    let r2 = List.map r2.chars ~f:priority |> List.sort ~compare:Poly.compare in
    let r3 = List.map r3.chars ~f:priority |> List.sort ~compare:Poly.compare in
    let rec aux r1 r2 r3 =
      match (r1, r2, r3) with
      | hd1 :: _, hd2 :: _, hd3 :: _ when hd1 = hd2 && hd2 = hd3 -> hd1
      | hd1 :: _, hd2 :: rest2, _ :: _ when hd1 > hd2 -> aux r1 rest2 r3
      | hd1 :: _, _ :: _, hd3 :: rest3 when hd1 > hd3 -> aux r1 r2 rest3
      | _ :: rest1, _, _ -> aux rest1 r2 r3
      | _ -> assert false
    in
    aux r1 r2 r3
end

let rec part1 acc = function
  | [] | "" :: [] -> acc
  | hd :: rest ->
      let r = Rucksack.of_string hd in
      let priorities = Rucksack.inter r |> List.map ~f:priority in
      let priority = List.fold ~init:0 ~f:( + ) priorities in
      part1 (acc + priority) rest

let rec part2 acc = function
  | l1 :: l2 :: l3 :: rest ->
      let r1 = Rucksack.of_string l1 in
      let r2 = Rucksack.of_string l2 in
      let r3 = Rucksack.of_string l3 in
      part2 (acc + Rucksack.badge r1 r2 r3) rest
  | _ -> acc

let () =
  printf "%d\n" (part1 0 input);
  printf "%d\n" (part2 0 input)
