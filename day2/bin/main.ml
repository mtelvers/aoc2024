let read_input =
  let ic = open_in "input" in
  let rec loop input lines =
    try
      let line = input_line input in
      loop input (line :: lines)
    with
    | End_of_file ->
        close_in input;
        lines
  in
  loop ic []

let reports = List.map (fun line -> String.split_on_char ' ' line |> List.map int_of_string) read_input

let rec deltas = function
  | []
  | _ :: [] -> []
  | r1 :: r2 :: tl -> (r2 - r1) :: deltas (r2 :: tl)

let rec bounds = function
  | [] -> true
  | d :: tl -> let d = Int.abs d in (d >= 1 && d <= 3) && bounds tl

let rec direction = function
  | []
  | _ :: [] -> true
  | d1 :: d2 :: tl -> (if d1 > 0 then d2 > 0 else d2 < 0) && direction (d2 :: tl)

let part1 = reports |> List.filter (fun rep ->
    let () = List.iter (Printf.printf "%i,") rep in
    let () = Printf.printf " -> " in
    let ds = deltas rep in
    let () = List.iter (Printf.printf "%i,") ds in
    let r = direction ds && bounds ds in
    let () = Printf.printf " -> %s\n" (if r then "true" else "false") in r
  ) |> List.length

let () = Printf.printf "part 1: %i\n" part1
