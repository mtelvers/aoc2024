let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

module StringMap = Map.Make (String)

module ListSet = Set.Make (struct
  type t = string list

  let compare = compare
end)

let computers =
  List.fold_left
    (fun map line ->
      match String.split_on_char '-' line with
      | n1 :: n2 :: _ -> StringMap.add_to_list n1 n2 map |> StringMap.add_to_list n2 n1
      | _ -> assert false)
    StringMap.empty input

let print graph =
  let oc = open_out "graph.gv" in
  let () = Printf.fprintf oc "strict graph g1 {\n" in
  let () = Printf.fprintf oc "  layout=\"fdp\";\n" in
  let () = Printf.fprintf oc "  overlay=\"scale\";\n" in
  let () = StringMap.iter (fun n0 nl -> List.iter (fun n1 -> Printf.fprintf oc "  %s -- %s;\n" n0 n1) nl) graph in
  let () = Printf.fprintf oc "}\n" in
  let () = close_out oc in
  Sys.command (Filename.quote_command "fdp" [ "-Tpdf"; "graph.gv" ] ~stdout:"graph.pdf")

let _ = print computers

let rec bfs d visited results =
  let nodes = StringMap.filter (fun _ (_, v) -> v = d) visited in
  let visited, results =
    StringMap.fold
      (fun c (lst, d) (map, set) ->
        StringMap.find c computers
        |> List.fold_left
             (fun (map, set) n ->
               match StringMap.find_opt n map with
               | Some (lst2, dep) ->
                   let set = if dep + d + 1 = 3 then ListSet.add (List.sort compare (lst @ List.rev (List.tl lst2))) set else set in
                   (map, set)
               | None -> (StringMap.add n (lst @ [ n ], d + 1) map, set))
             (map, set))
      nodes (visited, results)
  in
  if d < 10 then bfs (d + 1) visited results else results

let set = StringMap.fold (fun computer _ set -> bfs 0 (StringMap.singleton computer ([ computer ], 0)) set) computers ListSet.empty
let part1 = ListSet.filter (List.fold_left (fun acc c -> String.get c 0 = 't' || acc) false) set |> ListSet.cardinal
let () = Printf.printf "part 1: %i\n" part1

module StringSet = Set.Make (String)

let cliques =
  StringMap.mapi
    (fun computer _ ->
      let clique =
        StringMap.fold
          (fun comp _ set ->
            let candidate_friends = StringMap.find comp computers in
            if StringSet.fold (fun computer acc -> acc && List.mem computer candidate_friends) set true then StringSet.add comp set else set)
          computers (StringSet.singleton computer)
      in
      clique)
    computers

let maximal =
  StringMap.fold (fun _ clique maximal -> if StringSet.cardinal clique > StringSet.cardinal maximal then clique else maximal) cliques StringSet.empty

let () = StringSet.iter (Printf.printf "%s,") maximal
let () = Printf.printf "\n"
