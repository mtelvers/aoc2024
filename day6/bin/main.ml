let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type coord = {
  y : int;
  x : int;
}

module Lab = Map.Make (struct
  type t = coord

  let compare = compare
end)

let lab =
  input |> List.mapi (fun y line -> List.init (String.length line) (String.get line) |> List.mapi (fun x ch -> ({ y; x }, ch))) |> List.flatten |> Lab.of_list

let start, _ = lab |> Lab.filter (fun _ ch -> ch = '^') |> Lab.choose

let card = Lab.cardinal lab

let rec walk pos dir lab limit =
  if limit = 0 then []
  else
    let new_pos = { y = pos.y + dir.y; x = pos.x + dir.x } in
    match Lab.find_opt new_pos lab with
    | None -> []
    | Some '#' -> walk pos { y = dir.x; x = -dir.y } lab limit
    | Some '^'
    | Some '.' ->
        new_pos :: walk new_pos dir lab (limit - 1)
    | _ -> []

let part1 = walk start { y = -1; x = 0 } lab card |> List.sort_uniq compare |> List.length
let () = Printf.printf "part 1: %i\n" part1

let part2 =
  Lab.fold
    (fun p ch sum ->
      match ch with
      | '.' ->
          let new_lab = Lab.add p '#' lab in
          let len = walk start { y = -1; x = 0 } new_lab card |> List.length in
          sum + if len = card then 1 else 0
      | _ -> sum)
    lab 0

let () = Printf.printf "part 2: %i\n" part2
