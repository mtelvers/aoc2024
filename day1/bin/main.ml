let read_input =
  let ic = open_in "input" in
  let rec loop input lines =
    try
      let line = input_line input in
      loop input (line :: lines)
    with End_of_file ->
      close_in input;
      lines
  in
  loop ic []

let lst1, lst2 = List.fold_left (fun lst line ->
    (Scanf.sscanf line " %i %i " (fun i j -> (i, j))) :: lst
  ) [] read_input |> List.split

let sum = List.fold_left2 (fun sum i j -> sum + Int.abs (i - j)) 0 (List.sort compare lst1) (List.sort compare lst2)

let () = Printf.printf "part 1: %i\n" sum

let () = Printf.printf "part 2: %i\n" sum
