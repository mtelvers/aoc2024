let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type coord = {
  y : int;
  x : int;
}

module CoordMap = Map.Make (struct
  type t = coord

  let compare = compare
end)

let find map x = map |> CoordMap.filter (fun _ ch -> ch = x) |> CoordMap.choose |> fun (pos, _) -> pos

let map_of_strings s =
  s |> List.mapi (fun y line -> List.init (String.length line) (String.get line) |> List.mapi (fun x ch -> ({ y; x }, ch))) |> List.flatten |> CoordMap.of_list

let keypad = map_of_strings [ "789"; "456"; "123"; " 0A" ]
let keypad2 = map_of_strings [ " ^A"; "<v>" ]
let robot = find keypad 'A'
let avoid = find keypad ' '
let robot2 = find keypad2 'A'
let avoid2 = find keypad2 ' '
let human = robot2
let code = List.hd input
let code = List.init (String.length code) (String.get code)

let rec move avoid r t =
  if r.x < t.x && { r with x = r.x + 1 } <> avoid then '>' :: move avoid { r with x = r.x + 1 } t
  else if r.x > t.x && { r with x = r.x - 1 } <> avoid then '<' :: move avoid { r with x = r.x - 1 } t
  else if r.y < t.y && { r with y = r.y + 1 } <> avoid then 'v' :: move avoid { r with y = r.y + 1 } t
  else if r.y > t.y && { r with y = r.y - 1 } <> avoid then '^' :: move avoid { r with y = r.y - 1 } t
  else [ 'A' ]

let rec loop keypad avoid robot = function
  | [] -> []
  | hd :: tl ->
      let next = find keypad hd in
      let moves = move avoid robot next in
      moves @ loop keypad avoid next tl

let sequence = loop keypad avoid robot code
let () = List.iter (Printf.printf "%c") sequence
let () = Printf.printf "\n"

let sequence = loop keypad2 avoid2 robot2 sequence
let () = List.iter (Printf.printf "%c") sequence
let () = Printf.printf "\n"

let sequence = loop keypad2 avoid2 human sequence
let () = List.iter (Printf.printf "%c") sequence
let () = Printf.printf "\n"

