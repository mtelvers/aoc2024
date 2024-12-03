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
  |> List.filter_map (function
       | Str.Text _ -> None
       | Delim x ->
           let r = Str.regexp {|[(,)]|} in
           Some (Str.split r x |> List.filter_map Int.of_string_opt |> List.fold_left (fun sum x -> sum * x) 1))
  |> List.fold_left (fun t x -> t + x) 0

let () = Printf.printf "part 1: %i\n" part1
