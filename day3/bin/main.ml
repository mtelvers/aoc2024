let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic |> String.concat ""

module Int = struct
  include Int

  let of_string_opt s =
    try Some (int_of_string s) with
    | Failure _ -> None
end

let part1 =
  let r = Str.regexp {|mul([0-9]+,[0-9]+)|} in
  Str.full_split r input
  |> List.fold_left
       (fun acc s ->
         match s with
         | Str.Text _ -> acc
         | Delim x ->
             let r = Str.regexp {|[(,)]|} in
             let mul = Str.split r x |> List.filter_map Int.of_string_opt |> List.fold_left (fun sum x -> sum * x) 1 in
             acc + mul)
       0

let () = Printf.printf "part 1: %i\n" part1

type t = {
  add : int;
  total : int;
}

let part2 =
  let r = Str.regexp {|mul([0-9]+,[0-9]+)\|do()\|don't()|} in
  Str.full_split r input
  |> List.fold_left
       (fun acc s ->
         match s with
         | Str.Text _ -> acc
         | Delim "do()" -> { acc with add = 1 }
         | Delim "don't()" -> { acc with add = 0 }
         | Delim x ->
             let r = Str.regexp {|[(,)]|} in
             let mul = Str.split r x |> List.filter_map Int.of_string_opt |> List.fold_left (fun sum x -> sum * x) 1 in
             { acc with total = acc.total + (acc.add * mul) })
       { add = 1; total = 0 }

let () = Printf.printf "part 2: %i\n" part2.total
