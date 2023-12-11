open Base
open Stdio

let input = Advent.read_lines "data/2023/day07.txt"

module Card = struct
  type t = Number of int | T | J | Q | K | A

  let of_char = function
    | 'A' -> A
    | 'K' -> K
    | 'Q' -> Q
    | 'J' -> J
    | 'T' -> T
    | '0' .. '9' as c -> Char.to_string c |> Int.of_string |> Number
    | _ as c -> failwith ("Invalid card: " ^ Char.to_string c)

  let to_int ~joker_rule = function
    | A -> 14
    | K -> 13
    | Q -> 12
    | J -> if joker_rule then 0 else 11
    | T -> 10
    | Number a -> a

  let compare a b ~joker_rule =
    Int.compare (to_int a ~joker_rule) (to_int b ~joker_rule)
end

module Hand = struct
  type t = { cards : Card.t list; value : int; joker_rule : bool }

  type rank =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard

  let of_string line ~joker_rule =
    match String.split line ~on:' ' with
    | [ cards; value ] ->
        let value = Int.of_string value in
        let cards = String.to_list cards |> List.map ~f:Card.of_char in
        { cards; value; joker_rule }
    | _ -> failwith ("Invalid hand: " ^ line)

  let count_cards hand =
    let joker_rule = hand.joker_rule in
    let card_amount_compare (a_card, a_amount) (b_card, b_amount) =
      let compare_amount = Int.compare a_amount b_amount in
      if compare_amount <> 0 then compare_amount
      else Card.compare a_card b_card ~joker_rule
    in
    hand.cards
    |> List.sort_and_group ~compare:(Card.compare ~joker_rule)
    |> List.map ~f:(fun l -> (List.hd_exn l, List.length l))
    |> List.sort ~compare:card_amount_compare
    |> List.rev

  let rank hand =
    let joker_rule = hand.joker_rule in
    match count_cards hand with
    | [ (_, 5) ] -> FiveOfAKind
    | [ (_, 4); (J, 1) ] when joker_rule -> FiveOfAKind
    | [ (J, 4); (_, 1) ] when joker_rule -> FiveOfAKind
    | [ (_, 4); (_, 1) ] -> FourOfAKind
    | [ (_, 3); (J, 2) ] when joker_rule -> FiveOfAKind
    | [ (J, 3); (_, 2) ] when joker_rule -> FiveOfAKind
    | [ (_, 3); (_, 2) ] -> FullHouse
    | [ (J, 3); (_, 1); (_, 1) ] when joker_rule -> FourOfAKind
    | [ (_, 3); (_, 1); (J, 1) ] when joker_rule -> FourOfAKind
    | [ (_, 3); (_, 1); (_, 1) ] -> ThreeOfAKind
    | [ (_, 2); (J, 2); (_, 1) ] when joker_rule -> FourOfAKind
    | [ (_, 2); (_, 2); (J, 1) ] when joker_rule -> FullHouse
    | [ (_, 2); (_, 2); (_, 1) ] -> TwoPair
    | [ (J, 2); (_, 1); (_, 1); (_, 1) ] when joker_rule -> ThreeOfAKind
    | [ (_, 2); (_, 1); (_, 1); (J, 1) ] when joker_rule -> ThreeOfAKind
    | [ (_, 2); (_, 1); (_, 1); (_, 1) ] -> OnePair
    | [ (_, 1); (_, 1); (_, 1); (_, 1); (J, 1) ] when joker_rule -> OnePair
    | [ (_, 1); (_, 1); (_, 1); (_, 1); (_, 1) ] -> HighCard
    | _ -> failwith "Invalid hand"

  let to_int hand =
    match rank hand with
    | FiveOfAKind -> 7
    | FourOfAKind -> 6
    | FullHouse -> 5
    | ThreeOfAKind -> 4
    | TwoPair -> 3
    | OnePair -> 2
    | HighCard -> 1

  let compare a b =
    let joker_rule = a.joker_rule in
    let rank_compare = Int.compare (to_int a) (to_int b) in
    if rank_compare <> 0 then rank_compare
    else
      let cards =
        List.init (List.length a.cards) ~f:(fun idx ->
            (List.nth_exn a.cards idx, List.nth_exn b.cards idx))
      in
      match
        List.find cards ~f:(fun (a, b) -> Card.compare a b ~joker_rule <> 0)
      with
      | Some (a, b) -> Card.compare ~joker_rule a b
      | None -> 0
end

let rec parse_input hands ~joker_rule = function
  | [] -> hands
  | hd :: rest ->
      parse_input (Hand.of_string ~joker_rule hd :: hands) rest ~joker_rule

let solve input ~joker_rule =
  let hands = parse_input [] input ~joker_rule in
  List.sort hands ~compare:Hand.compare
  |> List.foldi ~init:0 ~f:(fun idx acc hand -> acc + ((idx + 1) * hand.value))

let () =
   printf "%d\n" (solve input ~joker_rule:false);
   printf "%d\n" (solve input ~joker_rule:true)
