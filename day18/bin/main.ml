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

let () =
  for y = 0 to size - 1 do
    for x = 0 to size - 1 do
      match CoordMap.find_opt { x; y } memory with
      | Some c -> print_char c
      | None -> print_char '.'
    done;
    print_char '\n'
  done

let print v =
  for y = 0 to size - 1 do
    for x = 0 to size - 1 do
      match CoordMap.find_opt { x; y } v with
      | Some i -> Printf.printf " %03i" i
      | None -> Printf.printf "    "
    done;
    print_char '\n'
  done

let rec loop depth visited =
  let () = Printf.printf "loop\n" in
  let () = print visited in
  let options = CoordMap.filter (fun _ d -> d = depth) visited in
  let visited = CoordMap.fold
    (fun pos d acc ->
      let options =
        [ { x = pos.x + 1; y = pos.y }; { x = pos.x - 1; y = pos.y }; { x = pos.x; y = pos.y - 1 }; { x = pos.x; y = pos.y + 1 } ]
        |> List.filter (fun p -> p.x >= 0 && p.x < size && p.y >= 0 && p.y < size)
        |> List.filter (fun p -> (not (CoordMap.mem p memory)) && not (CoordMap.mem p visited))
      in
(*
      let () = Printf.printf "size %i\n" (List.length options) in
      let () = List.iter (fun p -> Printf.printf "(%i,%i)," p.x p.y) options in
      let () = Printf.printf "\n" in
*)
      List.fold_left (fun acc p -> CoordMap.add p (d + 1) acc) acc options)
    options visited in
  match CoordMap.find_opt { x = size - 1; y = size - 1 } visited with
  | None -> loop (depth + 1) visited
  | Some d -> d

let steps = loop 0 CoordMap.(empty |> add { x = 0; y = 0 } 0)
let () = Printf.printf "part 1: %i\n" steps
