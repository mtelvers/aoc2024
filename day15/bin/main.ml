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

let rec run wh r = function
  | [] -> wh
  | d :: tl -> (
      let () = Printf.printf "robot at %i,%i\n" r.x r.y in
      let _ = print wh in
      let direction =
        match d with
        | '<' -> { x = -1; y = 0 }
        | '>' -> { x = 1; y = 0 }
        | '^' -> { x = 0; y = -1 }
        | 'v' -> { x = 0; y = 1 }
        | _ -> assert false
      in
      let rec loop push (p, ch) =
        let np = { y = p.y + direction.y; x = p.x + direction.x } in
        match CoordMap.find_opt np wh with
        | Some '.' -> CoordMap.add np ch push
        | Some 'O' -> loop (CoordMap.add np ch push) (np, 'O')
        | Some '#' -> CoordMap.empty
        | Some _
        | None ->
            assert false
      in
      let moves = loop CoordMap.(empty |> add r '.') (r, '@') in
      match CoordMap.is_empty moves with
      | true -> run wh r tl
      | false -> run (CoordMap.union (fun _ _ b -> Some b) wh moves) { y = r.y + direction.y; x = r.x + direction.x } tl)

let robot = CoordMap.filter (fun _ ch -> ch = '@') warehouse |> CoordMap.choose |> fun (p, _) -> p
let warehouse = run warehouse robot directions

let gps =
  CoordMap.fold
    (fun p c sum ->
      match c with
      | 'O' -> sum + (100 * p.y) + p.x
      | _ -> sum)
    warehouse 0

let () = Printf.printf "part 1: %i\n" gps
