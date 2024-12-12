let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type coord = {
  y : int;
  x : int;
}

module CoordMap = Map.Make (struct
  type t = coord

  let compare = compare
end)

let garden_list = input |> List.mapi (fun y line -> List.init (String.length line) (String.get line) |> List.mapi (fun x ch -> ({ y; x }, ch))) |> List.flatten
let garden = CoordMap.of_list garden_list
let plants = garden_list |> List.split |> fun (_, plants) -> List.sort_uniq compare plants
let directions = [ { y = -1; x = 0 }; { y = 1; x = 0 }; { y = 0; x = 1 }; { y = 0; x = -1 } ]

let rec find_region pos (r, g) =
  directions
  |> List.fold_left
       (fun (nr, ng) delta ->
         let new_pos = { y = pos.y + delta.y; x = pos.x + delta.x } in
         match CoordMap.find_opt new_pos ng with
         | None -> (nr, ng)
         | Some _ -> find_region new_pos (CoordMap.add new_pos ' ' nr, CoordMap.remove new_pos ng))
       (CoordMap.add pos ' ' r, CoordMap.remove pos g)

let perimiter r =
  CoordMap.fold
    (fun pos _ sum ->
      sum
      + List.fold_left
          (fun acc delta ->
            match CoordMap.find_opt { y = pos.y + delta.y; x = pos.x + delta.x } r with
            | None -> acc + 1
            | Some _ -> acc)
          0 directions)
    r 0

let part1 =
  List.fold_left
    (fun total plant ->
      let plant_locations = garden |> CoordMap.filter (fun _ p -> plant = p) in
      let rec loop = function
        | g when CoordMap.cardinal g = 0 -> []
        | g ->
            let region, garden = find_region (CoordMap.choose g |> fun (p, _) -> p) (CoordMap.empty, g) in
            region :: loop garden
      in
      let regions = loop plant_locations in
      total
      + List.fold_left
          (fun acc r ->
            let perimiter = perimiter r in
            let area = CoordMap.cardinal r in
            let () = Printf.printf "plant %c perimiter %i area %i = %i\n" plant perimiter area (perimiter * area) in
            (perimiter * area) + acc)
          0 regions)
    0 plants

let () = Printf.printf "part 1: %i\n" part1
