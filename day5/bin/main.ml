let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

let rules, pages =
  input
  |> List.filter (fun s -> String.length s > 0)
  |> List.partition (fun s ->
         try String.index s '|' > 0 with
         | Not_found -> false)

let pages = List.map (fun p -> p |> String.split_on_char ',' |> List.map int_of_string) pages

let rec test = function
  | [] -> true
  | p1 :: tl ->
      List.fold_left
        (fun acc p2 ->
          let p = Printf.sprintf "%i|%i" p1 p2 in
          let pr = Printf.sprintf "%i|%i" p2 p1 in
          acc && if List.mem p rules then true else if List.mem pr rules then false else assert false)
        true tl
      && test tl

let part1 =
  List.fold_left
    (fun sum l ->
      if test l then
        let mid = List.length l / 2 in
        sum + List.nth l mid
      else sum)
    0 pages

let () = Printf.printf "part 1: %i\n" part1
