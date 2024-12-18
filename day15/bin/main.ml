let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type coord = {
  y : int;
  x : int;
}

module CoordMap = Map.Make (struct
  type t = coord

  let compare = compare
end)

let map, directions =
  input
  |> List.filter (fun line -> String.length line > 0)
  |> List.partition (fun line ->
         match String.index_opt line '#' with
         | Some _ -> true
         | None -> false)

let warehouse =
  map
  |> List.mapi (fun y line -> List.init (String.length line) (String.get line) |> List.mapi (fun x ch -> ({ y; x }, ch)))
  |> List.flatten |> CoordMap.of_list

let print w =
  CoordMap.fold
    (fun { y; _ } ch ly ->
      let () = if not (ly = y) then Printf.printf "\n" in
      let () = Printf.printf "%c" ch in
      y)
    w 0

let directions = directions |> String.concat "" |> fun s -> List.init (String.length s) (String.get s)
let _ = print warehouse
let () = List.iter (Printf.printf "%c") directions

let rec run wh = function
  | [] -> wh
  | direction :: tl ->
      let robot = CoordMap.filter (fun _ ch -> ch = '@') wh |> CoordMap.choose |> fun (p, _) -> p in
(*    let _ = print wh in
      let () = Printf.printf "robot at %i,%i move %c\n" robot.x robot.y direction in *)
      let d =
        match direction with
        | '<' -> { x = -1; y = 0 }
        | '>' -> { x = 1; y = 0 }
        | '^' -> { x = 0; y = -1 }
        | 'v' -> { x = 0; y = 1 }
        | _ -> assert false
      in
      let rec loop push =
        let push2 =
          CoordMap.fold
            (fun p _ block ->
              let np = { y = p.y + d.y; x = p.x + d.x } in
              match CoordMap.find_opt np wh with
              | Some '.' -> block
              | Some '[' ->
                  if CoordMap.is_empty block then block
                  else if d.y = 0 then CoordMap.add np '[' block
                  else CoordMap.add np '[' block |> CoordMap.add { np with x = np.x + 1 } ']'
              | Some ']' ->
                  if CoordMap.is_empty block then block
                  else if d.y = 0 then CoordMap.add np ']' block
                  else CoordMap.add np ']' block |> CoordMap.add { np with x = np.x - 1 } '['
              | Some 'O' -> if CoordMap.is_empty block then block else CoordMap.add np 'O' block
              | Some '#' -> CoordMap.empty
              | Some _
              | None ->
                  assert false)
            push push
        in
        if CoordMap.cardinal push2 > CoordMap.cardinal push then loop push2 else push2
      in
      let block = loop CoordMap.(empty |> add robot '@') in
      let wh = CoordMap.fold (fun p _ acc -> CoordMap.add p '.' acc) block wh in
      let wh = CoordMap.fold (fun p ch acc -> CoordMap.add { x = p.x + d.x; y = p.y + d.y } ch acc) block wh in
      run wh tl

let warehouse = run warehouse directions

let gps w =
  CoordMap.fold
    (fun p c sum ->
      match c with
      | 'O' -> sum + (100 * p.y) + p.x
      | '[' -> sum + (100 * p.y) + p.x
      | _ -> sum)
    w 0

let () = Printf.printf "part 1: %i\n" (gps warehouse)

let double_warehouse =
  map
  |> List.mapi (fun y line ->
         List.init (String.length line) (String.get line)
         |> List.mapi (fun x ch ->
                let p1 = { y; x = x * 2 } in
                let p2 = { y; x = (x * 2) + 1 } in
                match ch with
                | '.'
                | '#' ->
                    [ (p1, ch); (p2, ch) ]
                | 'O' -> [ (p1, '['); (p2, ']') ]
                | '@' -> [ (p1, ch); (p2, '.') ]
                | _ -> assert false))
  |> List.flatten |> List.flatten |> CoordMap.of_list

let warehouse = run double_warehouse directions
let _ = print warehouse
let () = Printf.printf "part 2: %i\n" (gps warehouse)
