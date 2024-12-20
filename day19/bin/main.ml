let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic
let patterns, designs = List.partition (fun line -> String.contains line ',') input
let patterns = String.concat "," patterns |> String.split_on_char ',' |> List.sort_uniq compare
let () = List.iter (Printf.printf "%s,") patterns
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "%s\n") designs
let cache = Hashtbl.create 1000000

let part1 =
  List.fold_left
    (fun sum design ->
      let rec loop design =
        match Hashtbl.find_opt cache design with
        | None ->
            let matches =
              List.filter_map
                (fun pattern ->
                  if String.starts_with ~prefix:pattern design then
                    Some (String.sub design (String.length pattern) (String.length design - String.length pattern))
                  else None)
                patterns
            in
            (*  let () = List.iter (Printf.printf "%s > %s\n" design) matches in *)
            let possible = List.fold_left (fun acc design -> if design = "" then acc + 1 else acc) 0 matches in
            List.fold_left
              (fun acc subdesign ->
                let p = loop subdesign in
                let () = Hashtbl.add cache subdesign p in
                acc + p)
              possible matches
        | Some p -> p
      in
      let () = Printf.printf "%s can be made in " design in
      let () = flush stdout in
      let possible = loop design in
      let () = Printf.printf "%i ways\n" possible in
      let () = flush stdout in
      sum + possible)
    0 designs

let () = Printf.printf "part 1: %i\n" part1
let () = Printf.printf "cache size = %i\n" (Hashtbl.length cache)
