(* open Base
open Stdio

let input = Advent.read_lines "data/2022/day05.txt"

let rec split_on_whitespace lines =
  let split_pos =
    let open String in
    match List.findi ~f:(fun _ el -> el = "") lines with
    | Some (idx, _) -> idx
    | None -> assert false (* did not find the whitestring, input malformed *)
  in
  List.split_n lines split_pos

let parse_crates input n_stacks =
  let stacks = Array.create ~len:n_stacks [] in
  let rec aux = function
    | _ :: [] | [] -> () (* last line contains the stacks names *)
    | line :: rest ->
        Array.iteri stacks ~f:(fun idx stack ->
            let crate_pos = (idx * 3) + (idx + 1) in
            printf "%s (%d)\n" line crate_pos;
            let crate = String.sub ~pos:crate_pos ~len:1 line in
            let stack =
              if not (String.equal crate " ") then crate :: stack else stack
            in
            stacks.(idx) <- stack);
        aux rest
  in
  aux input;
  Array.map ~f:List.rev stacks

let drop_whitelines = List.filter ~f:(fun el -> not (String.equal el ""))

(* move QTTY from ORIG to DEST *)
(*       1         3       5   *)
let rec perform_instructions crates = function
  | "" :: [] | [] -> ()
  | hd :: rest ->
      let instructions = List.to_array (String.split ~on:' ' hd) in
      let qtty = Int.of_string instructions.(1) in
      let orig = Int.of_string instructions.(3) - 1 in
      let dest = Int.of_string instructions.(5) - 1 in
      let to_move, remaining = List.split_n crates.(orig) qtty in
      crates.(dest) <- List.append crates.(dest) (List.rev to_move);
      crates.(orig) <- remaining;
      perform_instructions crates rest

let print_response crates =
  Array.iter
    ~f:(fun stack ->
      match List.hd stack with Some el -> print_string el | None -> ())
    crates;
  print_endline ""

let solve input =
  let crates, instructions = split_on_whitespace input in
  let crates = drop_whitelines crates in
  let instructions = drop_whitelines instructions in
  let stacks = String.split ~on:' ' (String.strip (List.last_exn crates)) in
  let stacks = drop_whitelines stacks in
  let crates = parse_crates crates (List.length stacks) in
  perform_instructions crates instructions;
  print_response crates *)
