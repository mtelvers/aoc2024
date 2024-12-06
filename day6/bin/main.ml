let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type coord = {
  y : int;
  x : int;
}

module IntMap = Map.Make (Int)

let int_of_coord { y; x } = x + Int.shift_left y 16
let coord_of_int n = { y = Int.shift_right n 16; x = Int.logand n 0xffff }

let lab =
  input
  |> List.mapi (fun y line -> List.init (String.length line) (String.get line) |> List.mapi (fun x ch -> (int_of_coord { y; x }, ch)))
  |> List.flatten |> IntMap.of_list

let start = lab |> IntMap.filter (fun _ ch -> ch = '^') |> IntMap.choose |> fun (start, _) -> coord_of_int start

let rec walk lab current dir path =
  let path = IntMap.add_to_list (int_of_coord current) dir path in
  let new_pos = { y = current.y + dir.y; x = current.x + dir.x } in
  match IntMap.find_opt (int_of_coord new_pos) lab with
  | None -> (false, path)
  | Some '#' -> walk lab current { y = dir.x; x = -dir.y } path
  | Some '^'
  | Some '.' -> (
      match IntMap.find_opt (int_of_coord new_pos) path with
      | Some lst when List.mem dir lst -> (true, path)
      | Some _
      | None ->
          walk lab new_pos dir path)
  | _ -> (false, path)

let _, path = walk lab start { y = -1; x = 0 } IntMap.empty
let part1 = IntMap.cardinal path
let () = Printf.printf "part 1: %i\n" part1

let part2 =
  IntMap.fold
    (fun pos _ sum ->
      let new_lab = IntMap.add pos '#' lab in
      let loop, _ = walk new_lab start { y = -1; x = 0 } IntMap.empty in
      sum + if loop then 1 else 0)
    path 0

let () = Printf.printf "part 2: %i\n" part2
