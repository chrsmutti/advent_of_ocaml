open Base
open Stdio

let input = Advent.read_lines "data/2023/day02.txt"

type set = { red : int; green : int; blue : int }
type game = { id : int; sets : set list }

exception Invalid_color of string
exception Invalid_cube of string

let set_within_limit set limit =
  if set.red > limit.red then false
  else if set.green > limit.green then false
  else if set.blue > limit.blue then false
  else true

let parse_set set =
  let cubes = String.split set ~on:',' in
  List.fold ~init:{ red = 0; blue = 0; green = 0 } cubes ~f:(fun current cube ->
      match String.split (String.strip cube) ~on:' ' with
      | [ count; color ] ->
          let count = Int.of_string count in
          let current =
            match color with
            | "red" -> { current with red = count }
            | "green" -> { current with green = count }
            | "blue" -> { current with blue = count }
            | _ ->
                raise (Invalid_color (Printf.sprintf "Invalid color: %s" color))
          in
          current
      | _ ->
          raise
            (Invalid_cube
               (Printf.sprintf "Cube not formatted correctly: %s" cube)))

exception Invalid_game of string

let parse_game line =
  match String.split line ~on:':' with
  | [ id; sets ] ->
      let id = String.split id ~on:' ' in
      let id =
        match id with
        | [ _game; id ] -> Int.of_string id
        | _ -> raise (Invalid_game (Printf.sprintf "Invalid game: %s" line))
      in
      let sets = String.split sets ~on:';' in
      Some { id; sets = List.map ~f:parse_set sets }
  | _ -> None

let game_within_limit game ~limit =
  List.fold game.sets ~init:true ~f:(fun current set ->
      set_within_limit set limit && current)

let game_max_set game =
  let max_set acc set =
    let acc = if set.red > acc.red then { acc with red = set.red } else acc in
    let acc =
      if set.green > acc.green then { acc with green = set.green } else acc
    in
    let acc =
      if set.blue > acc.blue then { acc with blue = set.blue } else acc
    in
    acc
  in
  List.fold game.sets ~init:{ red = 0; green = 0; blue = 0 } ~f:max_set

let rec part1 input limit acc =
  match input with
  | [] -> acc
  | hd :: rest ->
      let game = parse_game hd in
      let result =
        Option.map game ~f:(fun g ->
            if game_within_limit g ~limit then g.id else 0)
      in
      let result = Option.value result ~default:0 in
      part1 rest limit acc + result

let rec part2 input acc =
  match input with
  | [] -> acc
  | hd :: rest ->
      let game = parse_game hd in
      let max_set = Option.map game ~f:game_max_set in
      let power =
        Option.map max_set ~f:(fun set -> set.red * set.green * set.blue)
      in
      let result = Option.value power ~default:0 in
      part2 rest acc + result

let () =
  printf "%d\n" (part1 input { red = 12; green = 13; blue = 14 } 0);
  printf "%d\n" (part2 input 0)
