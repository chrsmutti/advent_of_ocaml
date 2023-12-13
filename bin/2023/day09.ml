open Base
open Stdio

let input = Advent.read_lines "data/2023/day09.txt"

let rec predict_right sequence =
  if List.fold sequence ~init:0 ~f:Int.( + ) = 0 then 0
  else
    let differences =
      List.filter_mapi sequence ~f:(fun idx x ->
          List.nth sequence (idx + 1) |> Option.map ~f:(fun y -> y - x))
    in
    List.last_exn sequence + predict_right differences

let rec predict_left sequence =
  if List.fold sequence ~init:0 ~f:Int.( + ) = 0 then 0
  else
    let differences =
      List.filter_mapi sequence ~f:(fun idx x ->
          List.nth sequence (idx + 1) |> Option.map ~f:(fun y -> y - x))
    in
    List.hd_exn sequence - predict_left differences

let solve input =
  let line_to_ints s =
    String.split s ~on:' ' |> List.map ~f:String.strip
    |> List.map ~f:Int.of_string
  in
  let input = List.map input ~f:line_to_ints in
  let right =
    List.map input ~f:predict_right |> List.fold ~init:0 ~f:Int.( + )
  in
  let left = List.map input ~f:predict_left |> List.fold ~init:0 ~f:Int.( + ) in
  (right, left)

let () =
  let r, l = solve input in
  printf "%d\n%d\n" r l
