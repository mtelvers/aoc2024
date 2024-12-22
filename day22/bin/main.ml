let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic
let next num = List.fold_left (fun num op -> op num lxor num mod 16777216) num [ (fun x -> x * 64); (fun x -> x / 32); (fun x -> x * 2048) ]
let secrets = List.map int_of_string input

let rec loop n = function
  | 0 -> n
  | i -> loop (next n) (i - 1)

let part1 = List.fold_left (fun sum secret -> sum + loop secret 2000) 0 secrets
let () = Printf.printf "%i\n" part1

module IntSet = Set.Make (Int)

let pack a b c d = (((a + 128) land 0xff) lsl 24) lor (((b + 128) land 0xff) lsl 16) lor (((c + 128) land 0xff) lsl 8) lor ((d + 128) land 0xff)

let rec loop2 set d1 d2 d3 n = function
  | 0 -> set
  | i ->
      let s = next n in
      let d4 = (n mod 10) - (s mod 10) in
      (*  let () = Printf.printf "%i %i %i %i\n" d1 d2 d3 d4 in *)
      let packed = pack d1 d2 d3 d4 in
      loop2 (IntSet.add packed set) d2 d3 d4 s (i - 1)

let all_sequences =
  List.fold_left
    (fun set secret ->
      let v1 = next secret in
      let v2 = next v1 in
      let v3 = next v2 in
      let v4 = next v3 in
      loop2 set ((v1 mod 10) - (v2 mod 10)) ((v2 mod 10) - (v3 mod 10)) ((v3 mod 10) - (v4 mod 10)) v4 1996)
    IntSet.empty secrets

let rec loop3 target d1 d2 d3 n = function
  | 0 -> 0
  | i ->
      let s = next n in
      (*  let () = Printf.printf "%i %i %i %i\n" d1 d2 d3 (n mod 10 - s mod 10) in *)
      let d4 = (n mod 10) - (s mod 10) in
      let packed = pack d1 d2 d3 d4 in
      if target = packed then s mod 10 else loop3 target d2 d3 d4 s (i - 1)

module T = Domainslib.Task

let pool = T.setup_pool ~num_domains:6 ()

let part2 =
  T.run pool (fun _ ->
      let results =
        IntSet.fold
          (fun sequence acc ->
            acc
            @ [
                ( sequence,
                  T.async pool (fun _ ->
                      List.fold_left
                        (fun sum secret ->
                          let v1 = next secret in
                          let v2 = next v1 in
                          let v3 = next v2 in
                          let v4 = next v3 in
                          sum + loop3 sequence ((v1 mod 10) - (v2 mod 10)) ((v2 mod 10) - (v3 mod 10)) ((v3 mod 10) - (v4 mod 10)) v4 1996)
                        0 secrets) );
              ])
          all_sequences []
      in
      List.fold_left
        (fun acc (sequence, promise) ->
          let bananas = T.await pool promise in
          let () = Printf.printf "%x %i (best %i)\n" sequence bananas acc in
          let () = flush stdout in
          if bananas > acc then bananas else acc)
        0 results)

let () = Printf.printf "%i\n" part2
