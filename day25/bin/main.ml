let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type schematic =
  | Key
  | Lock

let rec loop = function
  | [] -> []
  | "" :: tl -> loop tl
  | l1 :: l2 :: l3 :: l4 :: l5 :: l6 :: l7 :: tl ->
      let map = [| l1; l2; l3; l4; l5; l6; l7 |] in
      let schematic = List.fold_left (fun acc j -> String.init 7 (fun i -> String.get (Array.get map i) j) :: acc) [] [ 4; 3; 2; 1; 0 ] in
      let schematic =
        List.fold_left
          (fun (_, acc) line ->
            match line with
            | "......#" -> (Key, (acc * 10) + 0)
            | ".....##" -> (Key, (acc * 10) + 1)
            | "....###" -> (Key, (acc * 10) + 2)
            | "...####" -> (Key, (acc * 10) + 3)
            | "..#####" -> (Key, (acc * 10) + 4)
            | ".######" -> (Key, (acc * 10) + 5)
            | "######." -> (Lock, (acc * 10) + 5)
            | "#####.." -> (Lock, (acc * 10) + 4)
            | "####..." -> (Lock, (acc * 10) + 3)
            | "###...." -> (Lock, (acc * 10) + 2)
            | "##....." -> (Lock, (acc * 10) + 1)
            | "#......" -> (Lock, (acc * 10) + 0)
            | _ -> assert false)
          (Key, 0) schematic
      in
      schematic :: loop tl
  | _ -> assert false

let keys, locks = loop input |> List.partition (fun (t, _) -> t = Key)
let _, keys = List.split keys
let _, locks = List.split locks
let () = List.iter (fun key -> Printf.printf "key %i\n" key) keys
let () = List.iter (fun lock -> Printf.printf "lock %i\n" lock) locks

let rec check_fit key lock =
  match (key, lock) with
  | 0, 0 -> true
  | key, lock ->
    let k = key mod 10 in let l = lock mod 10 in
    k + l < 6 && check_fit (key / 10) (lock / 10)

let () =
  List.iter
    (fun lock ->
      List.iter
        (fun key ->
          let fit = check_fit key lock in
          Printf.printf "%05i %05i sum %i %s\n" lock key (key + lock) (if fit then "fit" else "overlap"))
        keys)
    locks

let part1 = List.fold_left (fun acc lock -> List.fold_left (fun acc key -> if check_fit key lock then acc + 1 else acc) acc keys) 0 locks
let () = Printf.printf "part 1: %i\n" part1
