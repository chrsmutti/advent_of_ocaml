(* https://github.com/tjdevries/advent_of_code/blob/master/2022/lib/advent.ml *)
let read_lines file =
  In_channel.with_open_text file In_channel.input_all
  |> Str.(split (regexp "\n"))
