let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

let rules, pages =
  input
  |> List.filter (fun s -> String.length s > 0)
  |> List.partition (fun s ->
         try String.index s '|' > 0 with
         | Not_found -> false)

let rules =
  List.map
    (fun r ->
      r |> String.split_on_char '|' |> List.map int_of_string |> function
      | [ p1; p2 ] -> (p1, p2)
      | _ -> assert false)
    rules

let pages = List.map (fun p -> p |> String.split_on_char ',' |> List.map int_of_string) pages
let rcompare a b = if List.mem (a, b) rules then -1 else if List.mem (b, a) rules then 1 else 0

let rec test = function
  | [] -> true
  | p1 :: tl -> List.fold_left (fun acc p2 -> acc && rcompare p1 p2 = -1) true tl && test tl

let part1, part2 =
  List.fold_left
    (fun (sum1, sum2) l ->
      let mid = List.length l / 2 in
      if test l then (sum1 + List.nth l mid, sum2)
      else
        let nl = List.sort rcompare l in
        (sum1, sum2 + List.nth nl mid))
    (0, 0) pages

let () = Printf.printf "part 1: %i\n" part1
let () = Printf.printf "part 2: %i\n" part2
