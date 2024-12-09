let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic |> List.hd

let diskmap = List.init (String.length input) (fun i -> int_of_string (String.make 1 (String.get input i)))

let disksize = List.fold_left (fun sum x -> sum + x) 0 diskmap

let disk = Array.make disksize None

let rec fill start count v =
  let () = Array.set disk start v in
  if count > 1 then fill (start + 1) (count - 1) v
  else count + start

let rec filldisk id i = function
  | [] -> ()
  | file :: [] -> 
    ignore(fill i file (Some id))
  | file :: space :: tl -> 
    let i = fill i file (Some id) in
    let i = fill i space None in
    filldisk (id + 1) i tl

let () = filldisk 0 0 diskmap

let print () =
  let () = Array.iter (function
    | Some x -> Printf.printf "%i" x
    | None -> Printf.printf ".") disk
  in Printf.printf "\n"

let () = print ()

let rec find_space pos =
  if Array.get disk pos = None then
    pos else find_space (pos + 1)

let rec find_file pos =
  if Array.get disk pos != None then
    pos else find_file (pos - 1)

let rec defrag i r =
  if r = i then () else
  let space = find_space i in
  let file = find_file r in
  let () = Array.set disk space (Array.get disk file) in
  let () = Array.set disk file None in
  defrag (space + 1) (file - 1)

let () = defrag 0 (disksize - 1)

let part1 = Array.mapi (fun i v -> match v with None -> 0 | Some v -> v * i) disk |> Array.fold_left (fun sum v -> sum + v) 0
let () = Printf.printf "part 1: %i\n" part1
