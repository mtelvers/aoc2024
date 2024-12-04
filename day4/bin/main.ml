let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type coord = {
  y : int;
  x : int;
}

module Puzzle = Map.Make (struct
  type t = coord

  let compare = compare
end)

let puzzle =
  input
  |> List.mapi (fun y line -> List.init (String.length line) (String.get line) |> List.mapi (fun x ch -> ({ y; x }, ch)))
  |> List.flatten |> Puzzle.of_list

let directions =
  [ { y = -1; x = -1 }; { y = -1; x = 0 }; { y = -1; x = 1 }; { y = 0; x = -1 }; { y = 0; x = 1 }; { y = 1; x = -1 }; { y = 1; x = 0 }; { y = 1; x = 1 } ]

let xmas = [ 'X'; 'M'; 'A'; 'S' ]

let rec search p d = function
  | [] -> true
  | ch :: tl -> (
      Puzzle.find_opt p puzzle |> function
      | None -> false
      | Some x when x = ch ->
          search { y = p.y + d.y; x = p.x + d.x } d tl
      | Some _ -> false)

let part1 =
  Puzzle.fold
    (fun p _ sum ->
      sum + (directions |> List.filter (fun d -> search p d xmas) |> List.length))
    puzzle 0

let () = Printf.printf "part 1: %i\n" part1
