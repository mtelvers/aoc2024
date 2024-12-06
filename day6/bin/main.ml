let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type coord = {
  y : int;
  x : int;
}

module CoordMap = Map.Make (struct
  type t = coord

  let compare = compare
end)

let lab =
  input
  |> List.mapi (fun y line -> List.init (String.length line) (String.get line) |> List.mapi (fun x ch -> ({ y; x }, ch)))
  |> List.flatten |> CoordMap.of_list

let start, _ = lab |> CoordMap.filter (fun _ ch -> ch = '^') |> CoordMap.choose

let rec walk lab current dir path =
  let path = CoordMap.add_to_list current dir path in
  let new_pos = { y = current.y + dir.y; x = current.x + dir.x } in
  match CoordMap.find_opt new_pos lab with
  | None -> (false, path)
  | Some '#' -> walk lab current { y = dir.x; x = -dir.y } path
  | Some '^'
  | Some '.' -> (
      match CoordMap.find_opt new_pos path with
      | Some lst when List.mem dir lst -> (true, path)
      | Some _
      | None ->
          walk lab new_pos dir path)
  | _ -> (false, path)

let _, path = walk lab start { y = -1; x = 0 } CoordMap.empty
let part1 = CoordMap.cardinal path
let () = Printf.printf "part 1: %i\n" part1

let part2 =
  CoordMap.fold
    (fun pos _ sum ->
      let new_lab = CoordMap.add pos '#' lab in
      let loop, _ = walk new_lab start { y = -1; x = 0 } CoordMap.empty in
      sum + if loop then 1 else 0)
    path 0

let () = Printf.printf "part 2: %i\n" part2
