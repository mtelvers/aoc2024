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

let left, right = List.fold_left (fun lst line -> Scanf.sscanf line " %i %i " (fun l r -> (l, r)) :: lst) [] read_input |> List.split
let sum = List.fold_left2 (fun sum l r -> sum + Int.abs (l - r)) 0 (List.sort compare left) (List.sort compare right)
let () = Printf.printf "part 1: %i\n" sum

let sum =
  List.fold_left
    (fun sum l ->
      let count = List.fold_left (fun n r -> if l = r then n + 1 else n) 0 right in
      sum + (l * count))
    0 left

let () = Printf.printf "part 2: %i\n" sum
