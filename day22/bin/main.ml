let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic
let next num = List.fold_left (fun num op -> op num lxor num mod 16777216) num [ (fun x -> x * 64); (fun x -> x / 32); (fun x -> x * 2048) ]
let secrets = List.map int_of_string input

let rec loop n = function
  | 0 -> n
  | i -> loop (next n) (i - 1)

let part1 = List.fold_left (fun sum secret -> sum + loop secret 2000) 0 secrets
let () = Printf.printf "%i\n" part1
