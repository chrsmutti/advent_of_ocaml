open Base
open Stdio

let input = Advent.read_lines "data/2023/day05.txt"

exception Invalid_line of string

module Category = struct
  type t =
    | Seed
    | Soil
    | Fertilizer
    | Water
    | Light
    | Temperature
    | Humidity
    | Location
  [@@deriving equal]

  exception Unknown_category of string
  exception Invalid_conversion

  let of_string string =
    match String.lowercase string with
    | "seed" -> Seed
    | "soil" -> Soil
    | "fertilizer" -> Fertilizer
    | "water" -> Water
    | "light" -> Light
    | "temperature" -> Temperature
    | "humidity" -> Humidity
    | "location" -> Location
    | _ -> raise (Unknown_category string)

  let destination t =
    match t with
    | Seed -> Soil
    | Soil -> Fertilizer
    | Fertilizer -> Water
    | Water -> Light
    | Light -> Temperature
    | Temperature -> Humidity
    | Humidity -> Location
    | Location -> raise Invalid_conversion
end

type entity_range = { category : Category.t; start : int; length : int }
type entity = { category : Category.t; value : int }

module Converter = struct
  type conversion = {
    source : Category.t;
    source_start : int;
    destination_start : int;
    range : int;
  }

  type t = { conversions : conversion list }

  let within_conversion_range conversion number =
    Int.between number ~low:conversion.source_start
      ~high:(conversion.source_start + conversion.range)

  let range_within_conversion_range conversion range_start range_length =
    let conversion_end = conversion.source_start + conversion.range in
    let range_end = range_start + range_length in
    Int.between range_start ~low:conversion.source_start ~high:conversion_end
    || Int.between conversion.source_start ~low:range_start ~high:range_end

  let convert t entity =
    let conversion =
      List.find t.conversions ~f:(fun c ->
          Category.equal c.source entity.category
          && within_conversion_range c entity.value)
    in
    match conversion with
    | Some conversion ->
        let offset = entity.value - conversion.source_start in
        {
          category = Category.destination entity.category;
          value = conversion.destination_start + offset;
        }
    | None -> { entity with category = Category.destination entity.category }

  let convert_range t entity_range =
    let { category = _; start; length } = entity_range in
    let category = Category.destination entity_range.category in
    let range_end = entity_range.start + entity_range.length in
    let conversions =
      List.filter t.conversions ~f:(fun c ->
          Category.equal c.source entity_range.category
          && range_within_conversion_range c start length)
    in
    let conversions =
      ref
      @@ List.map conversions ~f:(fun c ->
             let offset = c.destination_start - c.source_start in
             let conversion_end = c.destination_start + c.range in
             let start = Int.max c.destination_start (entity_range.start + offset) in
             let range_end = Int.min (range_end + offset) conversion_end in
             { category; start; length = range_end - start })
    in
    let min_conversion_start =
      List.map !conversions ~f:(fun x -> x.start)
      |> List.fold ~init:Int.max_value ~f:Int.min
    in
    let max_conversion_end =
      List.map !conversions ~f:(fun x -> x.start + length)
      |> List.fold ~init:Int.min_value ~f:Int.max
    in
    (if entity_range.start < min_conversion_start then
       let identity_start = entity_range.start in
       let identity_end = Int.min min_conversion_start range_end in
       let identity_length = identity_end - identity_start in
       conversions :=
         { category; start = identity_start; length = identity_length }
         :: !conversions
     else if range_end > max_conversion_end then
       let identity_start = Int.max max_conversion_end entity_range.start in
       let identity_end = range_end in
       let identity_length = identity_end - identity_start in
       conversions :=
         { category; start = identity_start; length = identity_length }
         :: !conversions);
    !conversions
end

let parse_seeds line =
  match String.split line ~on:':' with
  | [ _; seeds ] ->
      String.split seeds ~on:' ' |> List.map ~f:String.strip
      |> List.filter ~f:(fun s -> not (String.is_empty s))
      |> List.map ~f:Int.of_string
      |> List.map ~f:(fun value -> { category = Seed; value })
  | _ -> raise (Invalid_line line)

let parse_seeds_range line =
  match String.split line ~on:':' with
  | [ _; seeds ] ->
      String.split seeds ~on:' ' |> List.map ~f:String.strip
      |> List.filter ~f:(fun s -> not (String.is_empty s))
      |> List.map ~f:Int.of_string |> List.chunks_of ~length:2
      |> List.map ~f:(fun ranges ->
             match ranges with
             | [ range_start; length ] ->
                 { category = Seed; start = range_start; length }
             | _ -> raise (Invalid_line line))
  | _ -> raise (Invalid_line line)

let rec parse_conversion input conversions =
  let open Converter in
  let extract_values line =
    match String.split line ~on:' ' with
    | [ destination_start; source_start; range ] ->
        ( Int.of_string destination_start,
          Int.of_string source_start,
          Int.of_string range )
    | _ -> raise (Invalid_line line)
  in
  match input with
  | hd :: rest when String.is_empty hd -> parse_conversion rest conversions
  | hd :: rest ->
      let _ =
        Str.search_forward
          (Str.regexp {|\([A-za-z]+\)-to-\([A-za-z]+\) map:|})
          hd 0
      in
      let source = Str.matched_group 1 hd |> Category.of_string in
      let new_conversions =
        List.take_while ~f:(fun l -> not (String.is_empty l)) rest
        |> List.map ~f:extract_values
        |> List.map ~f:(fun (destination_start, source_start, range) ->
               { source; source_start; destination_start; range })
      in
      parse_conversion
        (List.drop_while rest ~f:(fun l -> not (String.is_empty l)))
        (List.concat [ conversions; new_conversions ])
  | [] -> conversions

let part2 input =
  let open Converter in
  let rec convert_to_location converter (entity_ranges : entity_range list) =
    List.map entity_ranges ~f:(fun entity_range ->
        match entity_range.category with
        | Location -> [ entity_range.start ]
        | _ ->
            convert_to_location converter
              (Converter.convert_range converter entity_range))
    |> List.concat
  in
  let seed_ranges = parse_seeds_range (List.hd_exn input) in
  let rest = List.sub input ~pos:1 ~len:(List.length input - 1) in
  let conversions = parse_conversion rest [] in
  let converter = { conversions } in
  let locations = convert_to_location converter seed_ranges in
  List.fold locations ~init:Int.max_value ~f:Int.min

let part1 input =
  let open Converter in
  let rec convert_to_location converter entity =
    match entity.category with
    | Location -> entity.value
    | _ -> convert_to_location converter (Converter.convert converter entity)
  in
  let seeds = parse_seeds (List.hd_exn input) in
  let rest = List.sub input ~pos:1 ~len:(List.length input - 1) in
  let conversions = parse_conversion rest [] in
  let converter = { conversions } in
  let locations = List.map seeds ~f:(convert_to_location converter) in
  List.fold locations ~init:Int.max_value ~f:Int.min

let () =
  printf "%d\n" (part1 input);
  printf "%d\n" (part2 input)
