let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic |> List.hd
let stones = input |> String.split_on_char ' ' |> List.map int_of_string

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

let rec length x =
  match x / 10 with
  | 0 -> 1
  | n -> 1 + length n

let rules x =
  if x = 0 then [ 1 ]
  else
    let len = length x in
    if len mod 2 = 0 then
      let power = pow 10 (len / 2) in
      [ x / power; x mod power ]
    else [ x * 2024 ]

let cache = Hashtbl.create 1000000

let rec loop lst = function
  | -1 -> 1
  | n ->
      List.fold_left
        (fun sum x ->
          match Hashtbl.find_opt cache (x, n) with
          | None ->
              let l = loop (rules x) (n - 1) in
              let () = Hashtbl.add cache (x, n) l in
              sum + l
          | Some ans -> sum + ans)
        0 lst

let () = Printf.printf "part 1: %i\n" (loop stones 25)
let () = Printf.printf "part 2: %i\n" (loop stones 75)
