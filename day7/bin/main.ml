let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

module Int = struct
  include Int

  let of_string_opt s =
    try Some (int_of_string s) with
    | Failure _ -> None
end

type equation = {
  answer : int;
  numbers : int list;
}

let equations =
  input
  |> List.map (fun line ->
         String.split_on_char ':' line |> function
         | [ answer; nums ] -> { answer = int_of_string answer; numbers = String.split_on_char ' ' nums |> List.filter_map Int.of_string_opt }
         | _ -> assert false)

let rec foo v sum = function
  | [] -> sum = v
  | hd :: tl -> foo (hd + v) sum tl || foo (hd * v) sum tl

let part1 = equations |> List.fold_left (fun sum e -> sum + if foo 0 e.answer e.numbers then e.answer else 0) 0
let () = Printf.printf "part 1: %i\n" part1
