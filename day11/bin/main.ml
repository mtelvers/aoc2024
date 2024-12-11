let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic |> List.hd
let stones = input |> String.split_on_char ' ' |> List.map int_of_string

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

let rules x =
  if x = 0 then [ 1 ]
  else
    let str = string_of_int x in
    let len = String.length str in
    if len mod 2 = 0 then
      let power = pow 10 (len / 2) in
      [ x / power; x mod power ]
    else [ x * 2024 ]

let rec loop lst = function
  | 0 -> lst
  | n -> loop (List.concat_map (fun stone -> rules stone) lst) (n - 1)

let part1 = loop stones 25
let () = Printf.printf "part 1: %i\n" (List.length part1)
