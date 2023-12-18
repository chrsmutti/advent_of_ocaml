open Base
open Stdio

let input = Advent.read_lines "data/2023/day12.txt"

let rec combinations slots n =
  if slots = 0 then []
  else if slots = 1 && n = 1 then [ [ '#' ] ]
  else if slots = 1 && n = 0 then [ [ '.' ] ]
  else
    let hash =
      combinations (slots - 1) (n - 1) |> List.map ~f:(fun c -> '#' :: c)
    in
    let dot = combinations (slots - 1) n |> List.map ~f:(fun c -> '.' :: c) in
    List.concat [ hash; dot ]

let to_groups line =
  let rec aux line groups =
    if List.length line = 0 then groups
    else
      let group = List.take_while line ~f:(fun c -> not (Char.equal c '.')) in
      let rest = List.drop line (List.length group + 1) in
      if List.length group > 0 then aux rest (List.length group :: groups)
      else aux rest groups
  in
  aux line [] |> List.map ~f:Int.to_string |> List.rev |> String.concat ~sep:","

let rec replace_placeholders line = function
  | [] -> line
  | hd :: rest ->
      let line =
        Str.replace_first (Str.regexp_string "?") (Char.to_string hd) line
      in
      replace_placeholders line rest

let part1 input =
  let rec aux acc = function
    | [] -> acc
    | hd :: rest -> (
        match String.split hd ~on:' ' with
        | [ placeholder; groups ] ->
            let slots =
              String.to_array placeholder |> Array.count ~f:(Char.equal '?')
            in
            let placed =
              String.to_array placeholder |> Array.count ~f:(Char.equal '#')
            in
            let total =
              String.split ~on:',' groups
              |> List.map ~f:Int.of_string
              |> List.fold ~init:0 ~f:Int.( + )
            in
            let n = total - placed in
            let c = combinations slots n in
            let replaced =
              c |> List.map ~f:(replace_placeholders placeholder)
            in
            let valid =
              List.filter replaced ~f:(fun r ->
                  String.equal (to_groups (String.to_list r)) groups)
            in
            aux (acc + List.length valid) rest
        | _ -> failwith ("Invalid input: " ^ hd))
  in
  aux 0 input

let () = printf "%d\n" (part1 input)
