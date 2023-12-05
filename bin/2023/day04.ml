open Base
open Stdio

let input = Advent.read_lines "data/2023/day04.txt"

type card = {
  id : int;
  winning_numbers : (int, Base.Int.comparator_witness) Base.Set.t;
  elf_numbers : (int, Base.Int.comparator_witness) Base.Set.t;
}

let cached_result = Hashtbl.create (module Int)
let cached_cards = Hashtbl.create (module Int)

exception Invalid_card of string
exception Invalid_numbers of string

let read_card line =
  match String.split line ~on:':' with
  | [ card; numbers ] ->
      let id =
        String.chop_prefix card ~prefix:"Card"
        |> Option.value ~default:"0" |> String.strip |> Int.of_string
      in
      let winning_numbers, elf_numbers =
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
            (winning_numbers, elf_numbers)
        | _ ->
            raise
              (Invalid_numbers (Printf.sprintf "invalid numbers: %s" numbers))
      in
      { id; winning_numbers; elf_numbers }
  | _ -> raise (Invalid_card (Printf.sprintf "invalid card: %s" line))

let fill_cards input =
  List.map input ~f:read_card
  |> List.iter ~f:(fun card ->
         Hashtbl.set cached_cards ~key:card.id ~data:(ref card))

let card_count card =
  Set.inter !card.winning_numbers !card.elf_numbers |> Set.length

let card_value card =
  match card_count card with 0 -> 0 | count -> Int.pow 2 (count - 1)

let part1 input =
  input
  |> List.map ~f:(fun card -> ref (read_card card))
  |> List.map ~f:card_value
  |> List.fold ~init:0 ~f:Int.( + )

let part2 cards =
  let acc = ref 0 in
  while not (Queue.is_empty cards) do
    match Queue.dequeue cards with
    | Some id ->
        let new_cards =
          match Hashtbl.find cached_result id with
          | Some value -> value
          | None ->
              let card = Hashtbl.find_exn cached_cards id in
              let data =
                card_count card |> List.init ~f:(fun idx -> !card.id + idx + 1)
              in
              Hashtbl.set cached_result ~key:!card.id ~data;
              data
        in
        acc := !acc + 1;
        Queue.enqueue_all cards new_cards
    | None -> ()
  done;
  !acc

let () =
  printf "%d\n" (part1 input);
  printf "%d\n"
    (fill_cards input;
     part2 (Queue.init (List.length input) ~f:(fun x -> x + 1)))
