let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type coord = {
  y : int;
  x : int;
}

module CharMap = Map.Make (Char)

let map_of_strings s =
  s |> List.mapi (fun y line -> List.init (String.length line) (String.get line) |> List.mapi (fun x ch -> (ch, { y; x }))) |> List.flatten |> CharMap.of_list

let keypad = map_of_strings [ "789"; "456"; "123"; " 0A" ]
let keypad2 = map_of_strings [ " ^A"; "<v>" ]

let move avoid r t =
  let horizontal = if r.x < t.x then List.init (t.x - r.x) (fun _ -> '>') else if r.x > t.x then List.init (r.x - t.x) (fun _ -> '<') else [] in
  let vertical = if r.y < t.y then List.init (t.y - r.y) (fun _ -> 'v') else if r.y > t.y then List.init (r.y - t.y) (fun _ -> '^') else [] in
  ((if { x = r.x; y = t.y } <> avoid then [ vertical @ horizontal @ [ 'A' ] ] else [])
  @ if { x = t.x; y = r.y } <> avoid then [ horizontal @ vertical @ [ 'A' ] ] else [])
  |> List.sort_uniq compare

let min_list = function
  | [] -> None
  | x :: xs -> Some (List.fold_left min x xs)

let rec loop sequence = function
  | [] -> List.length sequence
  | keypad :: tl ->
      let robot = CharMap.find 'A' keypad in
      let avoid = CharMap.find ' ' keypad in
      List.fold_left
        (fun (robot, len) sequence ->
          let next = CharMap.find sequence keypad in
          let options = move avoid robot next in
          let l = min_list (List.map (fun opt -> len + loop opt tl) options) in
          (next, Option.value ~default:9999 l))
        (robot, 0) sequence
      |> fun (_, res) -> res

let complexity =
  List.fold_left
    (fun acc code ->
      let code_as_int = Scanf.sscanf code " %iA " @@ fun i -> i in
      let code = List.init (String.length code) (String.get code) in
      let sequence = loop code [ keypad; keypad2; keypad2 ] in
      let complexity = code_as_int * sequence in
      let () = Printf.printf "%i * %i = %i\n" sequence code_as_int (sequence * code_as_int) in
      complexity + acc)
    0 input

let () = Printf.printf "part 1: %i\n" complexity
