open Base
open Stdio

let input = Advent.read_lines "data/2023/day08.txt"

type node = { left : string; right : string }

let rec parse_nodes nodes = function
  | [] -> nodes
  | hd :: rest -> (
      match String.split hd ~on:'=' with
      | [ node; children ] -> (
          let node = String.strip node in
          let children =
            String.strip
              ~drop:(fun c ->
                Char.equal c '(' || Char.equal c ')' || Char.is_whitespace c)
              children
          in
          match String.split children ~on:',' with
          | [ left; right ] ->
              let left = String.strip left in
              let right = String.strip right in
              parse_nodes
                (Map.add_exn nodes ~key:node ~data:{ left; right })
                rest
          | _ -> failwith ("Invalid node: " ^ hd))
      | _ -> failwith ("Invalid node: " ^ hd))

let rec gcd a b =
  let a, b = if a > b then (a, b) else (b, a) in
  let q = a / b in
  let r = a - (b * q) in
  if r = 0 then b else gcd b r

let lcm a b = a * b / gcd a b

let parse_input input =
  match input with
  | dirs :: _ :: rest ->
      let directions = String.to_list dirs in
      let nodes = parse_nodes (Map.empty (module String)) rest in
      (directions, nodes)
  | _ -> failwith "Invalid input"

let steps_from_node_to_suffix nodes directions starting_node ~suffix =
  let steps = ref 0 in
  let current_node = ref starting_node in
  while not (String.is_suffix !current_node ~suffix) do
    let directions = Sequence.hd_exn directions in
    steps := List.length directions + !steps;
    List.iter directions ~f:(fun d ->
        let node = Map.find_exn nodes !current_node in
        match d with
        | 'R' -> current_node := node.right
        | 'L' -> current_node := node.left
        | _ -> failwith ("Unknown direction: " ^ Char.to_string d))
  done;
  !steps

let part1 input =
  let directions, nodes = parse_input input in
  let directions = Sequence.repeat directions in
  steps_from_node_to_suffix nodes directions "AAA" ~suffix:"ZZZ"

let part2 input =
  let directions, nodes = parse_input input in
  let directions = Sequence.repeat directions in
  let starting_nodes =
    List.filter (Map.keys nodes) ~f:(String.is_suffix ~suffix:"A")
  in
  List.map starting_nodes
    ~f:(steps_from_node_to_suffix nodes directions ~suffix:"Z")
  |> List.fold ~init:1 ~f:lcm

let () =
  printf "%d\n" (part1 input);
  printf "%d\n" (part2 input)
