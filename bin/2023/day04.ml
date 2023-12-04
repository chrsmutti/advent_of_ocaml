open Base
open Stdio

let input = Advent.read_lines "data/2023/day04.txt"

type card = {
  _id : int;
  winning_numbers : (int, Base.Int.comparator_witness) Base.Set.t;
  elf_numbers : (int, Base.Int.comparator_witness) Base.Set.t;
}

let read_card line =
  match String.split line ~on:':' with
  | [ card; numbers ] ->
      let id =
        String.chop_prefix card ~prefix:"Card"
        |> Option.value ~default:"0" |> String.strip |> Int.of_string
      in
      let numbers =
        match String.split numbers ~on:'|' with
        | [ winning_numbers; elf_numbers ] ->
            let winning_numbers =
              String.split winning_numbers ~on:' '
              |> List.filter ~f:(fun x -> not (String.is_empty x))
              |> List.map ~f:Int.of_string
              |> Set.of_list (module Int)
            in
            let elf_numbers =
              String.split elf_numbers ~on:' '
              |> List.filter ~f:(fun x -> not (String.is_empty x))
              |> List.map ~f:Int.of_string
              |> Set.of_list (module Int)
            in
            Ok (winning_numbers, elf_numbers)
        | _ -> Error (Printf.sprintf "invalid numbers: %s" numbers)
      in
      Result.map numbers ~f:(fun (winning_numbers, elf_numbers) ->
          { _id = id; winning_numbers; elf_numbers })
  | _ -> Error (Printf.sprintf "invalid card: %s" line)

let card_value card =
  let intersection = Set.inter card.winning_numbers card.elf_numbers in
  let winning_count = Set.length intersection in
  if winning_count = 0 then 0 else Int.pow 2 (winning_count - 1)

let solve input =
  input |> List.map ~f:read_card
  |> List.map ~f:(Result.map ~f:card_value)
  |> List.fold ~init:0 ~f:(fun acc card ->
         match card with
         | Ok value -> acc + value
         | Error error ->
             eprintf "%s" error;
             acc)

let () = printf "%d\n" (solve input)
