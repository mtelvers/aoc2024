let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type coord = {
  x : int;
  y : int;
}

module CoordMap = Map.Make (struct
  type t = coord

  let compare = compare
end)

type reindeer = {
  pos : coord;
  dir : coord;
  cost : int;
  map : char CoordMap.t;
}

let maze_list = input |> List.mapi (fun y line -> List.init (String.length line) (String.get line) |> List.mapi (fun x ch -> ({ y; x }, ch))) |> List.flatten
let maze = CoordMap.of_list maze_list
let start = maze |> CoordMap.filter (fun _ ch -> ch = 'S') |> CoordMap.choose |> fun (start, _) -> start
let finish = maze |> CoordMap.filter (fun _ ch -> ch = 'E') |> CoordMap.choose |> fun (finish, _) -> finish

let rec loop r costs =
  let () = Printf.printf "herd %i\n" (List.length r) in
  let () = flush stdout in
  let reindeers =
    List.concat_map
      (fun r ->
        [ { y = 0; x = 1 }; { y = 1; x = 0 }; { y = 0; x = -1 }; { y = -1; x = 0 } ]
        |> List.filter_map (fun dir ->
               let pos = { y = r.pos.y + dir.y; x = r.pos.x + dir.x } in
               let cost = r.cost + if dir = r.dir then 1 else 1001 in
               match CoordMap.find_opt pos r.map with
               | Some 'E'
               | Some '.' -> (
                   match CoordMap.find_opt pos costs with
                   | None -> Some { pos; dir; cost; map = CoordMap.add pos '#' r.map }
                   | Some v -> if v > cost then Some { pos; dir; cost; map = CoordMap.add pos '#' r.map } else None)
               | _ -> None))
      r
  in
  let costs =
    List.fold_left
      (fun acc r ->
        match CoordMap.find_opt r.pos acc with
        | Some c -> if c > r.cost then CoordMap.add r.pos r.cost acc else acc
        | None -> CoordMap.add r.pos r.cost acc)
      costs reindeers
  in
  let reindeers = List.filter (fun r -> r.pos != finish) reindeers in
  if List.length reindeers > 0 then loop reindeers costs else costs

let costs = loop [ { pos = start; dir = { y = 0; x = 1 }; cost = 0; map = maze } ] CoordMap.empty
let () = Printf.printf "part 1: %i\n" (CoordMap.find finish costs)
