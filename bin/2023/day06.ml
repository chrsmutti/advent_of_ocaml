open Base
open Stdio

let input = Advent.read_lines "data/2023/day06.txt"

let parse_races times distances ~part1 =
  let parse_line l =
    let values =
      if part1 then
        String.split l ~on:' ' |> List.map ~f:String.strip
        |> List.filter ~f:(fun s -> not @@ String.is_empty s)
      else [ String.filter l ~f:(fun c -> not @@ Char.is_whitespace c) ]
    in
    values |> List.map ~f:Int.of_string
  in
  let times = String.drop_prefix times (String.length "Time:") in
  let distances = String.drop_prefix distances (String.length "Distance:") in
  let times = parse_line times in
  let distances = parse_line distances in
  assert (List.length times = List.length distances);
  List.zip_exn times distances

let winning_moves (time, distance) =
  let rec aux hold_time total_time distance winning ~f =
    let vel = hold_time in
    let traveled = vel * (total_time - hold_time) in
    if traveled > distance then
      aux (f hold_time) total_time distance (winning + 1) ~f
    else winning
  in
  let left = time / 2 in
  let right = left + 1 in
  let left = aux left time distance 0 ~f:(fun x -> x - 1) in
  let right = aux right time distance 0 ~f:(fun x -> x + 1) in
  left + right

let part1 = function
  | [ times; distances ] ->
      parse_races ~part1:true times distances
      |> List.map ~f:winning_moves
      |> List.fold ~init:1 ~f:Int.( * )
  | _ -> failwith "Invalid input"

let part2 = function
  | [ times; distances ] ->
      parse_races ~part1:false times distances |> List.map ~f:winning_moves |> List.hd_exn
  | _ -> failwith "Invalid input"

let () = printf "%d\n" (part1 input); printf "%d\n" (part2 input);
