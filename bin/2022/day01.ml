open Base
open Stdio

let input = Advent.read_lines "data/2022/day01.txt"

(* This solves both part1 and part2, as part1 just needs
   to grab the value of m1 *)
let rec solve input acc max =
  match input with
  | [] -> max
  | "" :: rest ->
      let m1, m2, m3 = max in
      solve rest 0
        (match (m1, m2, m3) with
        | m1, m2, _ when acc > m1 -> (acc, m1, m2)
        | m1, m2, _ when acc > m2 -> (m1, acc, m2)
        | m1, m2, m3 when acc > m3 -> (m1, m2, acc)
        | _, _, _ -> (m1, m2, m3))
  | hd :: rest -> solve rest (acc + Int.of_string hd) max

let () =
  let m1, m2, m3 = solve input 0 (0, 0, 0) in
  printf "%d\n" m1;
  printf "%d\n" (m1 + m2 + m3)
