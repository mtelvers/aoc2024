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
let cheats = Hashtbl.create 10000

let rec loop depth track visited cheat =
  let options = CoordMap.filter (fun _ d -> d = depth) visited in
  let visited =
    CoordMap.fold
      (fun pos d visited ->
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
        let () =
          [
            { x = pos.x + 2; y = pos.y };
            { x = pos.x - 2; y = pos.y };
            { x = pos.x; y = pos.y - 2 };
            { x = pos.x; y = pos.y + 2 };
            { x = pos.x + 1; y = pos.y + 1 };
            { x = pos.x - 1; y = pos.y - 1 };
            { x = pos.x + 1; y = pos.y - 1 };
            { x = pos.x - 1; y = pos.y + 1 };
          ]
          |> List.iter (fun p ->
                 match CoordMap.find_opt p cheat with
                 | Some v -> Hashtbl.add cheats (pos, p) (d + v)
                 | None -> ())
        in
        List.fold_left (fun acc p -> CoordMap.add p (d + 1) acc) visited options)
      options visited
  in
  match CoordMap.find_opt finish visited with
  | None -> loop (depth + 1) track visited cheat
  | Some _ -> visited

let without_cheating = loop 0 track CoordMap.(empty |> add start 0) CoordMap.empty
let best = CoordMap.cardinal without_cheating - 1
let steps_remaining = CoordMap.map (fun v -> 1 + best - v) without_cheating
let _ = loop 0 track CoordMap.(empty |> add start 0) steps_remaining

let part1 =
  Hashtbl.fold
    (fun (start, finish) v acc ->
      let () = Printf.printf "(%i,%i) -> (%i,%i) = %i\n" start.x start.y finish.x finish.y v in
      if best - v >= 100 then acc + 1 else acc)
    cheats 0

let () = Printf.printf "part 1: %i\n" part1
