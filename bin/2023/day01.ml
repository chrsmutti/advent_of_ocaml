open Base
open Stdio

let input = Advent.read_lines "data/2023/day01.txt"
let numerals = [ "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9" ]

let spelled =
  [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

let digit_to_int digit =
  match digit with
  | "one" -> 1
  | "two" -> 2
  | "three" -> 3
  | "four" -> 4
  | "five" -> 5
  | "six" -> 6
  | "seven" -> 7
  | "eight" -> 8
  | "nine" -> 9
  | _ -> Int.of_string digit

let rec line_to_digits_rev line ~digits ~valid_digits =
  match line with
  | "" -> digits
  | line ->
      let digit =
        List.find valid_digits ~f:(fun digit ->
            String.is_substring_at line ~substring:digit ~pos:0)
      in
      let digit = Option.map digit ~f:digit_to_int in
      line_to_digits_rev
        (String.suffix line (String.length line - 1))
        ~digits:(digit :: digits) ~valid_digits

let rec solve input valid_digits acc =
  match input with
  | [] -> acc
  | hd :: rest ->
      let digits = List.rev (line_to_digits_rev hd ~digits:[] ~valid_digits) in
      let digits = List.filter_map digits ~f:(fun x -> x) in
      let first = List.hd digits |> Option.value ~default:0 |> Int.( * ) 10 in
      let last = List.rev digits |> List.hd |> Option.value ~default:0 in
      solve rest valid_digits acc + (first + last)

let () =
  printf "%d\n" (solve input numerals 0);
  printf "%d\n" (solve input (List.concat [ numerals; spelled ]) 0)
