let read_input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic
let reports = List.map (fun line -> String.split_on_char ' ' line |> List.map int_of_string) read_input

let rec deltas = function
  | []
  | _ :: [] ->
      []
  | r1 :: r2 :: tl -> (r2 - r1) :: deltas (r2 :: tl)

let pbound x = x >= 1 && x <= 3
let nbound x = x >= -3 && x <= -1

let rec direction = function
  | [] -> true
  | d2 :: [] -> pbound d2 || nbound d2
  | d1 :: d2 :: tl -> (if d1 > 0 then pbound d1 && d2 > 0 else nbound d1 && d2 < 0) && direction (d2 :: tl)

let part1 = reports |> List.filter (fun rep -> deltas rep |> direction) |> List.length
let () = Printf.printf "part 1: %i\n" part1

let sublists lst =
  let rec loop l1 = function
    | [] -> []
    | hd :: l2 -> (l1 @ l2) :: loop (l1 @ [ hd ]) l2
  in
  loop [] lst

let part2 =
  reports
  |> List.filter (fun rep ->
         if deltas rep |> direction then true else sublists rep |> List.fold_left (fun acc subrep -> deltas subrep |> direction || acc) false)
  |> List.length

let () = Printf.printf "part 2: %i\n" part2
