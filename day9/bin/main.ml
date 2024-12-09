let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic |> List.hd
let diskmap = List.init (String.length input) (fun i -> int_of_string (String.make 1 (String.get input i)))
let disksize = List.fold_left (fun sum x -> sum + x) 0 diskmap
let disk = Array.make disksize None

let rec fill start count v =
  let () = Array.set disk start v in
  if count > 1 then fill (start + 1) (count - 1) v else count + start

let rec filldisk id i = function
  | [] -> ()
  | file :: [] -> ignore (fill i file (Some id))
  | file :: space :: tl ->
      let i = fill i file (Some id) in
      let i = fill i space None in
      filldisk (id + 1) i tl

let () = filldisk 0 0 diskmap

let print () =
  let () =
    Array.iter
      (function
        | Some x -> Printf.printf "%i" x
        | None -> Printf.printf ".")
      disk
  in
  Printf.printf "\n"

let () = print ()
let rec find_space pos = if Array.get disk pos = None then pos else find_space (pos + 1)
let rec find_file pos = if Array.get disk pos != None then pos else find_file (pos - 1)

let rec defrag i r =
  if r = i then ()
  else
    let space = find_space i in
    let file = find_file r in
    let () = Array.set disk space (Array.get disk file) in
    let () = Array.set disk file None in
    defrag (space + 1) (file - 1)

let () = defrag 0 (disksize - 1)

let part1 =
  Array.mapi
    (fun i v ->
      match v with
      | None -> 0
      | Some v -> v * i)
    disk
  |> Array.fold_left (fun sum v -> sum + v) 0

let () = Printf.printf "part 1: %i\n" part1

type f = {
  id : int;
  size : int;
}

type foo =
  [ `File of f
  | `Space of int
  ]

let rec chunks id = function
  | [] -> []
  | size :: [] -> [ `File { id; size }; `Space 0 ]
  | size :: space :: tl -> `File { id; size } :: `Space space :: chunks (id + 1) tl

let chunklist = chunks 0 diskmap
let disk2 = chunklist |> Array.of_list

let print () =
  let () = Array.iter (function
    | `File {id; size} -> Printf.printf "f %i (%i), " id size
    | `Space size -> Printf.printf "s (%i), " size) disk2 in
  Printf.printf "\n"

let () = print ()

let len = Array.fold_left (fun sum v ->
    match v with
  | `File {id;size} -> sum + size
  | `Space size -> sum + size) 0

let disk_length = len disk2

let size_of_space i = match Array.get disk2 i with
                 | `Space n -> n
                 | _ -> 0

let () =
  List.rev chunklist
  |> List.iter (function
       | `Space _ -> ()
       | `File f -> (
           let space =
             Array.find_index
               (function
                 | `File _ -> false
                 | `Space n -> n >= f.size)
               disk2
           in
           match space with
           | None -> (* can't move *) ()
           | Some i ->
               let file_index =
                 Array.find_index
                   (fun v ->
                     match v with
                     | `File f2 -> f2.id = f.id
                     | _ -> false)
                   disk2 |> Option.value ~default:0
               in
               if file_index > i then
               let size_of_target_space = size_of_space i in
               let size_of_space_before_file = size_of_space (file_index - 1) in
               let size_of_space_after_file = size_of_space (file_index + 1) in 
               let () = Printf.printf "move file %i at index %i to space at index %i\n" f.id file_index i in
               let () = if file_index - i > 1 then Array.blit disk2 (i + 1) disk2 (i + 3) (file_index - i - 2) in
               let () = Array.set disk2 i (`Space 0) in
               let () = Array.set disk2 (i + 1) (`File f) in
               let () = Array.set disk2 (i + 2) (`Space (size_of_target_space - f.size)) in
               let () = if file_index - i = 1 then Array.set disk2 (file_index + 1) (`Space (size_of_space_before_file + size_of_space_after_file))
                   else Array.set disk2 (file_index + 1) (`Space (size_of_space_before_file + f.size + size_of_space_after_file)) in
               assert (disk_length = len disk2)
               else ()
         ))

let _ = Array.fold_left (fun i v ->
    match v with
  | `File {id;size} -> fill i size (Some id)
  | `Space size -> fill i size None) 0 disk2

let part2 =
  Array.mapi
    (fun i v ->
      match v with
      | None -> 0
      | Some v -> v * i)
    disk
  |> Array.fold_left (fun sum v -> sum + v) 0
let () = Printf.printf "part 2: %i\n" part2
