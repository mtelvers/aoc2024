let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type coord = {
  y : int;
  x : int;
}

module CoordMap = Map.Make (struct
  type t = coord

  let compare = compare
end)

let track =
  input
  |> List.mapi (fun y line -> List.init (String.length line) (String.get line) |> List.mapi (fun x ch -> ({ y; x }, ch)))
  |> List.flatten |> CoordMap.of_list

let start = track |> CoordMap.filter (fun _ ch -> ch = 'S') |> CoordMap.choose |> fun (start, _) -> start
let finish = track |> CoordMap.filter (fun _ ch -> ch = 'E') |> CoordMap.choose |> fun (finish, _) -> finish

let rec loop depth track visited =
  let options = CoordMap.filter (fun _ d -> d = depth) visited in
  let visited =
    CoordMap.fold
      (fun pos d acc ->
        let options =
          [ { x = pos.x + 1; y = pos.y }; { x = pos.x - 1; y = pos.y }; { x = pos.x; y = pos.y - 1 }; { x = pos.x; y = pos.y + 1 } ]
          |> List.filter (fun p ->
                 let on_track =
                   match CoordMap.find_opt p track with
                   | Some 'S'
                   | Some 'E'
                   | Some '.' ->
                       true
                   | Some '#'
                   | None ->
                       false
                   | Some _ -> assert false
                 in
                 on_track && not (CoordMap.mem p visited))
        in
        List.fold_left (fun acc p -> CoordMap.add p (d + 1) acc) acc options)
      options visited
  in
  match CoordMap.find_opt finish visited with
  | None -> loop (depth + 1) track visited
  | Some d -> d

let without_cheating = loop 0 track CoordMap.(empty |> add start 0)
let bottom_right, _ = CoordMap.max_binding track
let top_left, _ = CoordMap.min_binding track

let internal_walls =
  CoordMap.filter
    (fun pos ch ->
      match ch with
      | '#' -> not (pos.x = top_left.x || pos.y = top_left.y || pos.x = bottom_right.x || pos.y = bottom_right.y)
      | _ -> false)
    track

let part1 =
  let all_cheats =
    CoordMap.mapi
      (fun pos _ ->
        let cheat_track = CoordMap.add pos '.' track in
        let best = loop 0 cheat_track CoordMap.(empty |> add start 0) in
        let () = Printf.printf "(%i,%i) -> %i\n" pos.x pos.y best in
        let () = flush stdout in
        best)
      internal_walls
  in
  CoordMap.fold (fun _ v acc -> if without_cheating - v >= 100 then acc + 1 else acc) all_cheats 0

let () = Printf.printf "part 1: %i\n" part1
