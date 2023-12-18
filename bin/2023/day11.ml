open Base
open Stdio

let input = Advent.read_lines "data/2023/day11.txt"

module Position = struct
  type t = { x : int; y : int } [@@deriving equal]
end

let shortest_path a b =
  let open Position in
  Int.abs (a.x - b.x) + Int.abs (a.y - b.y)

let expand_universe input ~expansion =
  let rec aux coordinates last_coordinate = function
    | [] -> coordinates
    | line :: rest ->
        let coordinate =
          if List.exists line ~f:(fun c -> Char.equal c '#') then
            last_coordinate + 1
          else last_coordinate + expansion
        in
        aux (coordinate :: coordinates) coordinate rest
  in
  let y_coordinates = aux [] (-1) input |> List.rev |> List.to_array in
  let x_coordinates =
    aux [] (-1) (List.transpose_exn input) |> List.rev |> List.to_array
  in
  (x_coordinates, y_coordinates)

let find_galaxies input coordinates =
  let x_coordinates, y_coordinates = coordinates in
  let rec aux y galaxies = function
    | [] -> galaxies
    | line :: rest ->
        let open Position in
        let line_galaxies =
          List.mapi line ~f:(fun idx c -> (idx, c))
          |> List.filter ~f:(fun (_, c) -> Char.equal c '#')
          |> List.map ~f:(fun (idx, _) ->
                 { x = x_coordinates.(idx); y = y_coordinates.(y) })
        in
        let galaxies = List.concat [ line_galaxies; galaxies ] in
        aux (y + 1) galaxies rest
  in
  aux 0 [] input

let solve galaxies =
  let rec aux a bs steps =
    let steps =
      steps
      + (List.map bs ~f:(shortest_path a) |> List.fold ~init:0 ~f:Int.( + ))
    in
    match bs with a :: bs -> aux a bs steps | _ -> steps
  in
  match galaxies with a :: bs -> aux a bs 0 | _ -> 0

let () =
  let input = List.map input ~f:String.to_list in
  let part1 = expand_universe input ~expansion:2 in
  let galaxies = find_galaxies input part1 in
  printf "%d\n" (solve galaxies);
  let part2 = expand_universe input ~expansion:1_000_000 in
  let galaxies = find_galaxies input part2 in
  printf "%d\n" (solve galaxies)
