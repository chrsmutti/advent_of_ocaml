open Base
open Stdio

let input = Advent.read_lines "data/2022/day02.txt"

module Shape = struct
  type t = Rock | Paper | Scissors

  let of_theirs = function
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> assert false

  let of_ours = function
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissors
    | _ -> assert false

  let loses_to = function Rock -> Paper | Paper -> Scissors | Scissors -> Rock

  let wins_against = function
    | Rock -> Scissors
    | Paper -> Rock
    | Scissors -> Paper

  let score = function Rock -> 1 | Paper -> 2 | Scissors -> 3
end

open Shape

module Outcome = struct
  type t = Win | Draw | Loss

  let define = function
    | Rock, Scissors -> Win
    | Paper, Rock -> Win
    | Scissors, Paper -> Win
    | Scissors, Rock -> Loss
    | Rock, Paper -> Loss
    | Paper, Scissors -> Loss
    | _ -> Draw

  let of_string = function
    | "X" -> Loss
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> assert false

  let score = function Win -> 6 | Draw -> 3 | Loss -> 0
end

let overall_score (ours, theirs) =
  let outcome_score = Outcome.define (ours, theirs) |> Outcome.score in
  let shape_score = Shape.score ours in
  outcome_score + shape_score

let rec part1 acc = function
  | [] | "" :: [] -> acc
  | hd :: rest ->
      let theirs = String.sub hd ~pos:0 ~len:1 |> Shape.of_theirs in
      let ours = String.sub hd ~pos:2 ~len:1 |> Shape.of_ours in
      part1 (acc + overall_score (ours, theirs)) rest

let rec part2 acc = function
  | [] | "" :: [] -> acc
  | hd :: rest ->
      let theirs = String.sub hd ~pos:0 ~len:1 |> Shape.of_theirs in
      let outcome = String.sub hd ~pos:2 ~len:1 |> Outcome.of_string in
      let ours =
        match outcome with
        | Win -> theirs |> Shape.loses_to
        | Draw -> theirs
        | Loss -> theirs |> Shape.wins_against
      in
      part2 (acc + overall_score (ours, theirs)) rest

let () =
  printf "%d\n" (part1 0 input);
  printf "%d\n" (part2 0 input)
