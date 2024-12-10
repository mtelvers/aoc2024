let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type coord = {
  y : int;
  x : int;
}

let int_of_coord { y; x } = x + Int.shift_left y 16
let coord_of_int n = { y = Int.shift_right n 16; x = Int.logand n 0xffff }

module IntMap = Map.Make (Int)

let lab =
  input |> List.mapi (fun y line -> List.init (String.length line) (String.get line) |> List.mapi (fun x ch -> (int_of_coord{ y; x }, ch))) |> List.flatten |> IntMap.of_list

let start = lab |> IntMap.filter (fun _ ch -> ch = '^') |> IntMap.choose |> fun (start, _) -> coord_of_int start

let rec walk pos dir lab =
  let new_pos = { y = pos.y + dir.y; x = pos.x + dir.x } in
  match IntMap.find_opt (int_of_coord new_pos) lab with
  | None -> []
  | Some '#' -> walk pos { y = dir.x; x = -dir.y } lab
  | Some '^'
  | Some '.' ->
      new_pos :: walk new_pos dir lab
  | _ -> []

let rec count pos dir lab limit =
  if limit = 0 then limit
  else
    let new_pos = { y = pos.y + dir.y; x = pos.x + dir.x } in
    match IntMap.find_opt (int_of_coord new_pos) lab with
    | None -> 0
    | Some '#' -> count pos { y = dir.x; x = -dir.y } lab limit
    | Some '^'
    | Some '.' ->
        1 + count new_pos dir lab (limit - 1)
    | _ -> 0

let path = walk start { y = -1; x = 0 } lab |> List.sort_uniq compare
let part1 = path |> List.length
let () = Printf.printf "part 1: %i\n" part1

let max = part1 * 2

let part2 =
  List.fold_left
    (fun sum p ->
      let new_lab = IntMap.add (int_of_coord p) '#' lab in
      let len = count start { y = -1; x = 0 } new_lab max in
      sum + if len = max then 1 else 0)
    0 path

let () = Printf.printf "part 2: %i\n" part2
