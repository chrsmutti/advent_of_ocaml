open Base
open Stdio

let input = Advent.read_lines "data/2023/day04.txt"

module Scratchcard = struct
  type t = {
    winning : (int, Int.comparator_witness) Base.Set.t;
    drawn : (int, Int.comparator_witness) Base.Set.t;
  }

  exception Invalid_card of string
  exception Invalid_line of string

  let parse line =
    let parse_numbers numbers =
      String.split numbers ~on:' '
      |> List.filter ~f:(fun x -> not (String.is_empty x))
      |> List.map ~f:Int.of_string
      |> Set.of_list (module Int)
    in
    let parse_card card =
      match String.split card ~on:'|' with
      | [ winning; drawn ] ->
          let winning = parse_numbers winning in
          let drawn = parse_numbers drawn in
          { winning; drawn }
      | _ -> raise (Invalid_card card)
    in
    match String.split line ~on:':' with
    | [ _; card ] -> parse_card card
    | _ -> raise (Invalid_line line)

  let count c = Set.inter c.winning c.drawn |> Set.length
  let value c = match count c with 0 -> 0 | count -> Int.pow 2 (count - 1)
end

let part1 input =
  input
  |> List.map ~f:Scratchcard.parse
  |> List.map ~f:Scratchcard.value
  |> List.fold ~init:0 ~f:Int.( + )

let part2 input =
  let amounts = List.init (List.length input) ~f:(fun _ -> ref 1) in
  let cards = List.map input ~f:Scratchcard.parse in
  List.iteri cards ~f:(fun idx card ->
      let current_amount = List.nth_exn amounts idx in
      Scratchcard.count card
      |> List.init ~f:(fun x -> idx + x + 1)
      |> List.iter ~f:(fun idx ->
             let amount = List.nth_exn amounts idx in
             amount := !amount + !current_amount));
  List.map amounts ~f:Ref.( ! ) |> List.fold ~init:0 ~f:Int.( + )

let () =
  printf "%d\n" (part1 input);
  printf "%d\n" (part2 input)
