let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type coord = {
  y : int;
  x : int;
}

module CoordMap = Map.Make (struct
  type t = coord

  let compare = compare
end)

let corruptions = input |> List.map (fun line -> Scanf.sscanf line " %i , %i " @@ fun x y -> ({ x; y }, '#')) |> Array.of_list
let memory = Array.init 1024 (fun i -> Array.get corruptions i) |> Array.to_list |> CoordMap.of_list
let size = 71

let rec loop depth memory visited =
  let options = CoordMap.filter (fun _ d -> d = depth) visited in
  let visited =
    CoordMap.fold
      (fun pos d acc ->
        let options =
          [ { x = pos.x + 1; y = pos.y }; { x = pos.x - 1; y = pos.y }; { x = pos.x; y = pos.y - 1 }; { x = pos.x; y = pos.y + 1 } ]
          |> List.filter (fun p -> p.x >= 0 && p.x < size && p.y >= 0 && p.y < size)
          |> List.filter (fun p -> (not (CoordMap.mem p memory)) && not (CoordMap.mem p visited))
        in
        List.fold_left (fun acc p -> CoordMap.add p (d + 1) acc) acc options)
      options visited
  in
  match CoordMap.find_opt { x = size - 1; y = size - 1 } visited with
  | None -> loop (depth + 1) memory visited
  | Some d -> d

let steps = loop 0 memory CoordMap.(empty |> add { x = 0; y = 0 } 0)
let () = Printf.printf "part 1: %i\n" steps

let () =
  for n = 1025 to (Array.length corruptions) do
    let memory = Array.init n (fun i -> Array.get corruptions i) |> Array.to_list |> CoordMap.of_list in
    let last, _ = Array.get corruptions (n - 1) in
    let () = Printf.printf "(%i,%i) %i: " last.x last.y n in
    let () = flush stdout in
    let steps = loop 0 memory CoordMap.(empty |> add { x = 0; y = 0 } 0) in
    let () = Printf.printf "%i\n" steps in
    flush stdout
  done
