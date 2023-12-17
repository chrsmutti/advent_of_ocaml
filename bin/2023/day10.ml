open Base
open Stdio
open Ocolor_format

let input = Advent.read_lines "data/2023/day10.txt"

type direction = North | South | East | West

module Position = struct
  module T = struct
    type t = { x : int; y : int } [@@deriving compare, sexp_of, hash, equal]

    let east pos = ({ pos with x = pos.x + 1 }, West)
    let west pos = ({ pos with x = pos.x - 1 }, East)
    let north pos = ({ pos with y = pos.y - 1 }, South)
    let south pos = ({ pos with y = pos.y + 1 }, North)

    let check grid (pos, incoming_direction) =
      match (grid.(pos.y).(pos.x), incoming_direction) with
      | 'S', _ -> Some pos
      | '-', East | '-', West -> Some pos
      | '|', North | '|', South -> Some pos
      | 'L', North | 'L', East -> Some pos
      | 'J', North | 'J', West -> Some pos
      | '7', South | '7', West -> Some pos
      | 'F', South | 'F', East -> Some pos
      | _ -> None

    let paths grid pos =
      match grid.(pos.y).(pos.x) with
      | 'S' ->
          List.filter_opt
            [
              check grid (east pos);
              check grid (west pos);
              check grid (north pos);
              check grid (south pos);
            ]
      | '-' -> List.filter_opt [ check grid (east pos); check grid (west pos) ]
      | '|' ->
          List.filter_opt [ check grid (north pos); check grid (south pos) ]
      | 'L' -> List.filter_opt [ check grid (east pos); check grid (north pos) ]
      | 'J' -> List.filter_opt [ check grid (west pos); check grid (north pos) ]
      | '7' -> List.filter_opt [ check grid (west pos); check grid (south pos) ]
      | 'F' -> List.filter_opt [ check grid (east pos); check grid (south pos) ]
      | _ -> []

    let within pos grid =
      pos.y < Array.length grid && pos.x < Array.length grid.(pos.y)
  end

  include T
  include Comparator.Make (T)
end

let rec find_start grid pos =
  let open Position in
  match grid.(pos.y).(pos.x) with
  | 'S' -> pos
  | _ ->
      if pos.x + 1 < Array.length grid.(pos.y) then
        find_start grid { pos with x = pos.x + 1 }
      else find_start grid { x = 0; y = pos.y + 1 }

let to_grid input =
  let padding =
    String.init (String.length (List.hd_exn input)) ~f:(fun _ -> '.')
  in
  let input = List.concat [ [ padding ]; input; [ padding ] ] in
  List.map input ~f:(fun s -> String.to_array ("." ^ s ^ ".")) |> List.to_array

let main_loop = ref @@ Set.empty (module Position)

let rec traverse grid positions k =
  let positions = List.dedup_and_sort positions ~compare:Position.compare in
  if List.length positions = 0 then k
  else
    let next =
      List.map positions ~f:(fun pos ->
          let next = Position.paths grid pos in
          if List.length next > 0 then main_loop := Set.add !main_loop pos;
          next)
    in
    let next =
      List.concat next
      |> List.filter ~f:(fun pos -> not (Set.mem !main_loop pos))
    in
    traverse grid next (k + 1)

type location = Outside | Inside | Loop

let rec check_inside grid =
  let open Position in
  for y = 0 to Array.length grid - 1 do
    let edges =
      String.of_array grid.(y)
      |> Str.global_replace (Str.regexp "FJ") "|." (* zig-zag *)
      |> Str.global_replace (Str.regexp "L7") "|." (* zig-zag *)
      |> Str.global_replace (Str.regexp "SJ") "|." (* zig-zag or u-turn *)
      |> Str.global_replace (Str.regexp "S7") "|." (* zig-zag or n-turn *)
      |> Str.global_replace (Str.regexp "FS") "|." (* zig-zag or n-turn *)
      |> Str.global_replace (Str.regexp "LS") "|." (* zig-zag or u-turn *)
      |> Str.global_replace (Str.regexp "LJ") "|." (* u-turn *)
      |> Str.global_replace (Str.regexp "F7") "|." (* u-turn *)
      |> String.to_array
      |> Array.filter_mapi ~f:(fun x c ->
             match c with
             | '|' | 'L' | 'J' | '7' | 'F' -> Some { x; y }
             | _ -> None)
      |> Array.filter ~f:(Set.mem !main_loop)
      |> Array.to_list
    in
    (* List.iter edges ~f:(fun e -> printf "(%d; %d) " e.x e.y); prerr_endline ""; *)
    for x = 0 to Array.length grid.(y) - 1 do
      let cur =
        match grid.(y).(x) with
        | '-' -> "─"
        | '|' -> "|"
        | 'L' -> "└"
        | 'J' -> "┘"
        | '7' -> "┐"
        | 'F' -> "┌"
        | c -> Char.to_string c
      in
      let edges_to_right =
        List.length @@ List.filter edges ~f:(fun pos -> pos.x > x)
      in
      let inside = edges_to_right % 2 <> 0 in
      if Set.mem !main_loop { x; y } then Ocolor_format.printf "@{<red>%s@}" cur
      else if inside then Ocolor_format.printf "@{<green>%s@}" cur
      else Ocolor_format.printf "%s" cur;
      Ocolor_format.pp_print_flush Ocolor_format.std_formatter ()
    done;
    print_endline ""
  done

let rec print_loop grid =
  let open Position in
  for y = 0 to Array.length grid - 1 do
    for x = 0 to Array.length grid.(y) - 1 do
      let cur =
        match grid.(y).(x) with
        | '-' -> "─"
        | '|' -> "|"
        | 'L' -> "└"
        | 'J' -> "┘"
        | '7' -> "┐"
        | 'F' -> "┌"
        | c -> Char.to_string c
      in
      if Set.mem !main_loop { x; y } then Ocolor_format.printf "@{<red>%s@}" cur
      else Ocolor_format.printf "%s" cur;
      Ocolor_format.pp_print_flush Ocolor_format.std_formatter ()
    done;
    print_endline ""
  done

let () =
  let grid = to_grid input in
  let start = find_start grid { x = 0; y = 0 } in
  let _ = traverse grid [ start ] 0 in
  check_inside grid
