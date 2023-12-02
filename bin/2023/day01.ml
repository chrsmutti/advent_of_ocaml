open Base
open Stdio

let input = Advent.read_lines "data/2023/day01.txt"

let spelled =
  [
    "one";
    "two";
    "three";
    "four";
    "five";
    "six";
    "seven";
    "eight";
    "nine";
    "oneight";
    "twone";
    "threeight";
    "fiveight";
    "eightwo";
    "eighthree";
    "nineight";
    "twoneight";
  ]

let spelled_to_int number =
  match number with
  | "one" -> Some 1
  | "two" -> Some 2
  | "three" -> Some 3
  | "four" -> Some 4
  | "five" -> Some 5
  | "six" -> Some 6
  | "seven" -> Some 7
  | "eight" -> Some 8
  | "nine" -> Some 9
  | "oneight" -> Some 18
  | "twone" -> Some 21
  | "threeight" -> Some 38
  | "fiveight" -> Some 58
  | "eightwo" -> Some 82
  | "eighthree" -> Some 83
  | "nineight" -> Some 98
  | "twoneight" -> Some 218
  | _ -> None

let replace_spelled line =
  let aux spelled line =
    let number = Stdlib.Option.get (spelled_to_int spelled) in
    Str.global_replace (Str.regexp_string spelled) (Int.to_string number) line
  in
  List.fold (List.rev spelled) ~init:line ~f:(fun acc s -> aux s acc)

let rec extract_numbers numbers first last =
  match numbers with
  | [] -> (first, last)
  | hd :: rest ->
      let hdi = hd in
      let first = if String.equal first "" then hdi else first in
      let last = hdi in
      extract_numbers rest first last

let rec solve input spelled acc =
  match input with
  | [] -> acc
  | hd :: rest ->
      let hd = if spelled then replace_spelled hd else hd in
      let hd = Str.global_replace (Str.regexp "[A-Za-z]") "" hd in
      let numbers = Str.split (Str.regexp "") hd in
      let first, last = extract_numbers numbers "" "" in
      let acc = acc + Int.of_string (String.concat (first :: [ last ])) in
      solve rest spelled acc

let () =
  printf "%d\n" (solve input false 0);
  printf "%d\n" (solve input true 0)
