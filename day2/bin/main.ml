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
  | _ :: [] ->
      []
  | r1 :: r2 :: tl -> (r2 - r1) :: deltas (r2 :: tl)

let rec bounds = function
  | [] -> true
  | d :: tl ->
      let d = Int.abs d in
      (d >= 1 && d <= 3) && bounds tl

let rec direction = function
  | []
  | _ :: [] ->
      true
  | d1 :: d2 :: tl -> (if d1 > 0 then d2 > 0 else d2 < 0) && direction (d2 :: tl)

let part1 =
  reports
  |> List.filter (fun rep ->
         let ds = deltas rep in
         direction ds && bounds ds)
  |> List.length

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
         let ds = deltas rep in
         let r = direction ds && bounds ds in
         if r then r
         else
           sublists rep
           |> List.fold_left
                (fun acc subrep ->
                  let ds = deltas subrep in
                  (direction ds && bounds ds) || acc)
                false)
  |> List.length

let () = Printf.printf "part 2: %i\n" part2
