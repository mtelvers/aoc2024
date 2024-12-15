let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type coord = {
  y : int;
  x : int;
}

module CoordMap = Map.Make (struct
  type t = coord

  let compare = compare
end)

let map, directions = input |> List.filter (fun line -> String.length line > 0) |> List.partition (fun line -> match String.index_opt line '#' with | Some _ -> true | None -> false)

let warehouse =
  map
  |> List.mapi (fun y line -> List.init (String.length line) (String.get line) |> List.mapi (fun x ch -> ({ y; x }, ch)))
  |> List.flatten |> CoordMap.of_list

let print w =
  CoordMap.fold (fun {y;_} ch ly ->
      let () = if not (ly = y) then Printf.printf "\n" in
      let () = Printf.printf "%c" ch in
      y
    ) w 0

let directions = directions |> String.concat "" |> fun s -> List.init (String.length s) (String.get s)
let _ = print warehouse

let () = List.iter (Printf.printf "%c") directions


let rec run wh r = function
  | [] -> wh
  | d :: tl ->
let () = Printf.printf "robot at %i,%i\n" r.x r.y in
let direction = match d with
  | '<' -> { x = -1; y = 0 }
  | '>' -> { x = 1; y = 0 }
  | '^' -> { x = 0; y = -1 }
  | 'v' -> { x = 0; y = 1 }
  | _ -> assert false in

let rec loop p w =
  let np = { y = p.y + direction.y; x = p.x + direction.x; } in
  match CoordMap.find_opt np w with
  | Some '.' -> ['.']
  | Some 'O' -> loop np w @ ['O']
  | Some '#' -> ['#']
  | Some _
  | None -> assert false in

let moves = (loop r wh) @ ['@'] in

match moves with
| []
| '#' :: _  -> run wh r tl
| hd :: tl2 ->
  let moves = hd :: List.rev tl2 in

let () = List.iter (fun ch ->
Printf.printf "move %c\n" ch
    ) moves in

let wh, _ = List.fold_left (fun (w, p) ch -> 
  (CoordMap.add p ch w,
  { y = p.y + direction.y; x = p.x + direction.x; })
        ) (wh, r) moves in

let _ = print wh in
  run wh { y = r.y + direction.y; x = r.x + direction.x} tl

let robot = CoordMap.filter (fun _ ch -> ch = '@') warehouse |> CoordMap.choose |> fun (p, _)  -> p

let warehouse = run warehouse robot directions

let gps = CoordMap.fold (fun p c sum ->
    match c with
    | 'O' -> sum + 100 * p.y + p.x
    | _ -> sum
  ) warehouse 0

let () = Printf.printf "part 1: %i\n" gps
