let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type coord = {
  y : int;
  x : int;
}

module CoordMap = Map.Make (struct
  type t = coord
  let compare = compare
end)

let city =
  input |> List.mapi (fun y line -> List.init (String.length line) (String.get line) |> List.mapi (fun x ch -> ({ y; x }, ch))) |> List.flatten |> CoordMap.of_list

let print m =
  CoordMap.fold (fun {y; _} ch last ->
      let () = if last != y then Printf.printf "\n" in
      let () = Printf.printf "%c" ch in
      y) m 0

let _ = print city

let antennas = city |> CoordMap.filter (fun _ ch -> ch != '.')

let freqs = antennas |> CoordMap.to_list |> List.split |> fun (_, f) -> List.sort_uniq compare f

let antimap r = freqs |> List.fold_left (fun acc f ->
    let locations, _ = antennas |> CoordMap.filter (fun _ ch -> ch = f) |> CoordMap.to_list |> List.split in
    let rec loop = function
     | [] 
     | _ :: [] -> []
     | a0 :: rest ->
       let rec repeat = function
         | 0 -> []
         | n ->
       (List.fold_left (fun acc a1 ->
       let d = { y = a0.y - a1.y; x = a0.x - a1.x } in
       [
         { y = a0.y + n * d.y; x = a0.x + n * d.x };
         { y = a0.y - n * d.y; x = a0.x - n * d.x };
         { y = a1.y + n * d.y; x = a1.x + n * d.x };
         { y = a1.y - n * d.y; x = a1.x - n * d.x }
       ] @ acc) [] rest) @ repeat (n - 1)
       in (repeat r) @ loop rest
    in
    let () = List.iter (fun l -> Printf.printf "(%i,%i)," l.x l.y) locations in
    let () = Printf.printf "\n" in
    let antis = loop locations |> List.sort_uniq compare |> List.filter (fun p -> not (List.mem p locations)) in
    let within_bounds = List.filter (fun l -> CoordMap.find_opt l city != None) antis in
    let new_city = List.fold_left (fun acc l -> CoordMap.add l '#' acc) acc within_bounds in
    let _ = print new_city in
    List.iter (fun l -> Printf.printf "(%i,%i)," l.x l.y) within_bounds;
    Printf.printf "\n";
    new_city
  ) city

let part1 = (antimap 1) |> CoordMap.filter (fun _ ch -> ch = '#') |> CoordMap.cardinal
let () = Printf.printf "part 1: %i\n" part1

let part2 = (CoordMap.cardinal city) - ((antimap 50) |> CoordMap.filter (fun _ ch -> ch = '.') |> CoordMap.cardinal)
let () = Printf.printf "part 2: %i\n" part2

