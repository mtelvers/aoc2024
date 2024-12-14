let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type coord = {
  y : int;
  x : int;
}

module CoordMap = Map.Make (struct
  type t = coord

  let compare = compare
end)

let choose_any_coord x = CoordMap.choose x |> fun (p, _) -> p
let garden_list = input |> List.mapi (fun y line -> List.init (String.length line) (String.get line) |> List.mapi (fun x ch -> ({ y; x }, ch))) |> List.flatten
let garden = CoordMap.of_list garden_list
let plants = garden_list |> List.split |> fun (_, plants) -> List.sort_uniq compare plants
let directions = [ { y = 0; x = 1 }; { y = 1; x = 0 }; { y = 0; x = -1 }; { y = -1; x = 0 } ]

let rec find_region pos (r, g) =
  directions
  |> List.fold_left
       (fun (nr, ng) delta ->
         let new_pos = { y = pos.y + delta.y; x = pos.x + delta.x } in
         match CoordMap.find_opt new_pos ng with
         | None -> (nr, ng)
         | Some _ -> find_region new_pos (CoordMap.add new_pos ' ' nr, CoordMap.remove new_pos ng))
       (CoordMap.add pos ' ' r, CoordMap.remove pos g)

let perimeter r =
  CoordMap.fold
    (fun pos _ acc ->
      List.fold_left
        (fun acc delta ->
          match CoordMap.find_opt { y = pos.y + delta.y; x = pos.x + delta.x } r with
          | None -> (
              (*
               *   x,y  N  x+1,y
               *      +->-+
               *      |   |
               *    W ^   v E
               *      |   |
               *      +-<-+
               * x,y+1  S x+1,y+1
               *)
              let { y; x } = pos in
              match delta with
              | { y = -1; x = 0 } (* North *) -> ({ y; x }, { y = 0; x = 1 }) :: acc
              | { y = 0; x = 1 } (* East *) -> ({ y; x = x + 1 }, { y = 1; x = 0 }) :: acc
              | { y = 1; x = 0 } (* South *) -> ({ y = y + 1; x = x + 1 }, { y = 0; x = -1 }) :: acc
              | { y = 0; x = -1 } (* West *) -> ({ y = y + 1; x }, { y = -1; x = 0 }) :: acc
              | _ -> acc)
          | Some _ -> acc)
        acc directions)
    r []

let direction_vector v = { y = (if v.y != 0 then v.y / abs v.y else 0); x = (if v.x != 0 then v.x / abs v.x else 0) }

let simplify lst =
  let rec loop lst = function
    | [] -> []
    | (p, d) :: tl -> (
        let np = { y = p.y + d.y; x = p.x + d.x } in
        match List.assoc_opt np tl with
        | None -> (p, d) :: loop lst tl
        | Some d2 ->
            if direction_vector d = direction_vector d2 then
              let np = { y = p.y + d.y; x = p.x + d.x } in
              (p, { y = d.y + d2.y; x = d.x + d2.x }) :: loop lst (List.filter (fun (_p, _d) -> not (_p = np && _d = d2)) tl)
            else (p, d) :: loop lst tl)
  in
  let rec repeat lst =
    let simple_list = loop lst lst |> List.rev in
    let simple_list = loop simple_list simple_list in
    if List.length simple_list = List.length lst then simple_list else repeat simple_list
  in
  repeat lst

let part1, part2 =
  List.fold_left
    (fun (part1, part2) plant ->
      let plant_locations = garden |> CoordMap.filter (fun _ p -> plant = p) in
      let rec loop = function
        | g when CoordMap.cardinal g = 0 -> []
        | g ->
            let region, garden = find_region (choose_any_coord g) (CoordMap.empty, g) in
            region :: loop garden
      in
      let regions = loop plant_locations in
      List.fold_left
        (fun (part1, part2) r ->
          let perimeter = perimeter r in
          let sides = List.length (simplify perimeter) in
          let perimeter = List.length perimeter in
          let area = CoordMap.cardinal r in
          ((perimeter * area) + part1, (sides * area) + part2))
        (part1, part2) regions)
    (0, 0) plants

let () = Printf.printf "part 1: %i\n" part1
let () = Printf.printf "part 2: %i\n" part2
