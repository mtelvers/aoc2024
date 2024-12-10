let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type coord = {
  y : int;
  x : int;
}

module CoordMap = Map.Make (struct
  type t = coord

  let compare = compare
end)

let island =
  input
  |> List.mapi (fun y line -> List.init (String.length line) (String.get line) |> List.mapi (fun x ch -> ({ y; x }, int_of_string (String.make 1 ch))))
  |> List.flatten |> CoordMap.of_list

let rec walk pos n =
  [ { y = -1; x = 0 }; { y = 1; x = 0 }; { y = 0; x = 1 }; { y = 0; x = -1 } ]
  |> List.filter_map (fun dir ->
         let new_pos = { y = pos.y + dir.y; x = pos.x + dir.x } in
         match CoordMap.find_opt new_pos island with
         | None -> None
         | Some h when h = n -> Some new_pos
         | Some _ -> None)
  |> fun next_steps ->
  match n with
  | 9 -> next_steps
  | _ -> List.concat_map (fun pos -> walk pos (n + 1)) next_steps

let trailheads = island |> CoordMap.filter (fun _ ch -> ch = 0) |> CoordMap.to_list |> List.split |> fun (pos, _) -> pos
let part1 = trailheads |> List.fold_left (fun sum p -> sum + List.length (walk p 1 |> List.sort_uniq compare)) 0
let () = Printf.printf "part 1: %i\n" part1

let rec walk2 pos n =
  [ { y = -1; x = 0 }; { y = 1; x = 0 }; { y = 0; x = 1 }; { y = 0; x = -1 } ]
  |> List.filter_map (fun dir ->
         let new_pos = { y = pos.y + dir.y; x = pos.x + dir.x } in
         match CoordMap.find_opt new_pos island with
         | None -> None
         | Some h when h = n -> Some new_pos
         | Some _ -> None)
  |> fun next_steps ->
  match n with
  | 9 -> List.length next_steps
  | _ -> List.fold_left (fun sum pos -> sum + walk2 pos (n + 1)) 0 next_steps

let trailheads = island |> CoordMap.filter (fun _ ch -> ch = 0) |> CoordMap.to_list |> List.split |> fun (pos, _) -> pos
let part2 = trailheads |> List.fold_left (fun sum p -> sum + (walk2 p 1)) 0
let () = Printf.printf "part 2: %i\n" part2
