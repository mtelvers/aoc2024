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
  input
  |> List.mapi (fun y line -> List.init (String.length line) (String.get line) |> List.mapi (fun x ch -> ({ y; x }, ch)))
  |> List.flatten |> CoordMap.of_list

let antennas = city |> CoordMap.filter (fun _ ch -> ch != '.')
let freqs = antennas |> CoordMap.to_list |> List.split |> fun (_, f) -> List.sort_uniq compare f

let antimap r =
  freqs
  |> List.fold_left
       (fun acc f ->
         let locations, _ = antennas |> CoordMap.filter (fun _ ch -> ch = f) |> CoordMap.to_list |> List.split in
         let rec loop = function
           | []
           | _ :: [] ->
               []
           | a0 :: rest ->
               let rec repeat n =
                 let extras =
                   List.fold_left
                     (fun acc a1 ->
                       let d = { y = a0.y - a1.y; x = a0.x - a1.x } in
                       [
                         { y = a0.y + (n * d.y); x = a0.x + (n * d.x) };
                         { y = a0.y - (n * d.y); x = a0.x - (n * d.x) };
                         { y = a1.y + (n * d.y); x = a1.x + (n * d.x) };
                         { y = a1.y - (n * d.y); x = a1.x - (n * d.x) };
                       ]
                       @ acc)
                     [] rest
                   |> List.filter (fun l -> CoordMap.find_opt l city != None)
                 in
                 if List.length extras > 0 then if r then extras @ repeat (n + 1) else extras else []
               in
               repeat 1 @ loop rest
         in
         let antis = loop locations |> List.sort_uniq compare |> List.filter (fun p -> not (List.mem p locations)) in
         let new_city = List.fold_left (fun acc l -> CoordMap.add l '#' acc) acc antis in
         new_city)
       city

let part1 = antimap false |> CoordMap.filter (fun _ ch -> ch = '#') |> CoordMap.cardinal
let () = Printf.printf "part 1: %i\n" part1
let part2 = CoordMap.cardinal city - (antimap true |> CoordMap.filter (fun _ ch -> ch = '.') |> CoordMap.cardinal)
let () = Printf.printf "part 2: %i\n" part2
