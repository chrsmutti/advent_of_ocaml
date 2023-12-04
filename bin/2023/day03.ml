open Base
open Stdio

let input = Advent.read_lines "data/2023/day03.txt"

module Position = struct
  module T = struct
    type t = { x : int; y : int } [@@deriving compare, sexp_of, hash]

    let adjacent pos =
      let adjx = [ { pos with x = pos.x - 1 }; { pos with x = pos.x + 1 } ] in
      let adjy = [ { pos with y = pos.y - 1 }; { pos with y = pos.y + 1 } ] in
      let adjdu =
        [ { x = pos.x - 1; y = pos.y + 1 }; { x = pos.x + 1; y = pos.y + 1 } ]
      in
      let adjdd =
        [ { x = pos.x - 1; y = pos.y - 1 }; { x = pos.x + 1; y = pos.y - 1 } ]
      in
      List.concat [ adjx; adjy; adjdu; adjdd ]
  end

  include T
  include Comparator.Make (T)
end

type symbol = { symbol : char; position : Position.t }

let numberstbl =
  Hashtbl.create
    ~size:(List.length input * String.length (List.hd_exn input))
    (module Position)

let rec parse_chars chars current_pos symbols =
  let open Position in
  match chars with
  | [] -> symbols
  | digit :: rest when Char.is_digit digit ->
      let other_digits = List.take_while rest ~f:Char.is_digit in
      let rest = List.drop rest (List.length other_digits) in
      let digits = digit :: other_digits in
      let number = Int.of_string (String.of_list digits) in
      let positions =
        List.init (List.length digits) ~f:(fun x ->
            { current_pos with x = current_pos.x + x })
      in
      List.iter positions ~f:(fun pos ->
          Hashtbl.set numberstbl ~key:pos ~data:number);
      parse_chars rest
        { current_pos with x = current_pos.x + List.length digits }
        symbols
  | point :: rest when Char.equal point '.' ->
      parse_chars rest { current_pos with x = current_pos.x + 1 } symbols
  | symbol :: rest ->
      let symbol = { symbol; position = current_pos } in
      parse_chars rest
        { current_pos with x = current_pos.x + 1 }
        (symbol :: symbols)

let get_parts pos =
  Position.adjacent pos
  |> List.filter_map ~f:(Hashtbl.find numberstbl)
  |> List.fold ~init:(Set.empty (module Int)) ~f:Set.add
  |> Set.to_list

let calc_part_numbers symbols =
  let get_parts' parts pos = List.concat [ parts; get_parts pos ] in
  List.map symbols ~f:(fun s -> s.position)
  |> List.fold ~init:[] ~f:get_parts'
  |> List.fold ~init:0 ~f:Int.( + )

let calc_gear_ratio symbols =
  List.filter symbols ~f:(fun symbol -> Char.equal symbol.symbol '*')
  |> List.map ~f:(fun s -> s.position)
  |> List.map ~f:get_parts
  |> List.filter ~f:(fun parts -> List.length parts = 2)
  |> List.map ~f:(List.fold ~init:1 ~f:Int.( * ))
  |> List.fold ~init:0 ~f:Int.( + )

let solve input calc_fun =
  List.mapi input ~f:(fun idx line -> (idx, String.to_list line))
  |> List.map ~f:(fun (idx, chars) -> parse_chars chars { x = 0; y = idx } [])
  |> List.concat |> calc_fun

let () =
  printf "%d\n" (solve input calc_part_numbers);
  printf "%d\n" (solve input calc_gear_ratio)
