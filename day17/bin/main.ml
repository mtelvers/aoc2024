let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type cpu = {
  oi : int;
  ip : int;
  a : int;
  b : int;
  c : int;
  found : bool;
}

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

let registers, memory = input |> List.filter (fun l -> String.length l > 0) |> List.partition (fun l -> String.split_on_char ' ' l |> List.length > 2)
let registers = String.concat ", " registers
let cpu = Scanf.sscanf registers " Register A: %i , Register B: %i , Register C: %i " @@ fun a b c -> { oi = 0; ip = 0; a; b; c; found = false }
let memory = String.concat "" memory
let memory = Scanf.sscanf memory " Program: %s " @@ fun p -> p
let memory = String.split_on_char ',' memory |> List.map int_of_string |> Array.of_list
let output = Array.copy memory

let operand cpu = function
  | 0 -> 0
  | 1 -> 1
  | 2 -> 2
  | 3 -> 3
  | 4 -> cpu.a
  | 5 -> cpu.b
  | 6 -> cpu.c
  | _ -> assert false

(*
let rec binary = function
  | 0 -> ()
  | x ->
      let () = binary (x lsr 1) in
      if x land 1 = 0 then print_char '0' else print_char '1'
*)

let rec loop cpu test =
  (* let () = Printf.printf "IP %i A %i B %i C %i\n" cpu.ip cpu.a cpu.b cpu.c in *)
  if cpu.ip = Array.length memory then cpu
  else
    match Array.get memory cpu.ip with
    | 0 (* adv *) ->
        let op = operand cpu (Array.get memory (cpu.ip + 1)) in
        loop { cpu with ip = cpu.ip + 2; a = cpu.a / pow 2 op } test
    | 1 (* bxl *) ->
        let op = Array.get memory (cpu.ip + 1) in
        loop { cpu with ip = cpu.ip + 2; b = cpu.b lxor op } test
    | 2 (* bst *) ->
        let op = operand cpu (Array.get memory (cpu.ip + 1)) in
        loop { cpu with ip = cpu.ip + 2; b = op mod 8 } test
    | 3 (* jnz *) ->
        let op = Array.get memory (cpu.ip + 1) in
        if cpu.a = 0 then loop { cpu with ip = cpu.ip + 2 } test else loop { cpu with ip = op } test
    | 4 (* bxc *) -> loop { cpu with ip = cpu.ip + 2; b = cpu.b lxor cpu.c } test
    | 5 (* out *) ->
        let op = operand cpu (Array.get memory (cpu.ip + 1)) mod 8 in
        let () = Array.set output cpu.oi op in
        if not test then loop { cpu with ip = cpu.ip + 2; oi = cpu.oi + 1 } test
        else if op = Array.get memory cpu.oi then
          if cpu.a = 0 && cpu.oi + 1 = Array.length memory then { cpu with found = true } else loop { cpu with ip = cpu.ip + 2; oi = cpu.oi + 1 } test
        else { cpu with oi = cpu.oi + 1 }
    | 6 (* bdv *) ->
        let op = operand cpu (Array.get memory (cpu.ip + 1)) in
        loop { cpu with ip = cpu.ip + 2; b = cpu.a / pow 2 op } test
    | 7 (* cdv *) ->
        let op = operand cpu (Array.get memory (cpu.ip + 1)) in
        loop { cpu with ip = cpu.ip + 2; c = cpu.a / pow 2 op } test
    | _ -> assert false

let cpu = loop cpu false
let () = Printf.printf "part 1: "

let () =
  for i = 0 to cpu.oi - 1 do
    Printf.printf "%i," (Array.get output i)
  done

let () = Printf.printf "\n"
let () = flush stdout

(* 
 * Program: 2,4,1,3,7,5,0,3,1,5,4,1,5,5,3,0
 * 
 *                                       Program: 2,4,1,3,7,5,0,3,1,5,4,1,5,5,3,0
 *                              100 =          4: 2,6,
 *                       1010111111 =        703: 2,4,1,7,
 *                     101010111111 =       2751: 2,4,1,3,3,
 *                    1101010100000 =       6816: 2,4,1,3,7,0,
 *             11000001101010100000 =     793248: 2,4,1,3,7,5,6,
 *          10111110110001010111111 =    6251199: 2,4,1,3,7,5,0,5,
 *      100101100010101111001101101 =   78732909: 2,4,1,3,7,5,0,3,2,
 * 10000101000100010101111001101101 = 2232508013: 2,4,1,3,7,5,0,3,1,4,
 * 10010101000100010101111001101101 = 2500943469: 2,4,1,3,7,5,0,3,1,5,5,
 *
 *)

let x = Hashtbl.create 1_000_000
let () = Hashtbl.add x 0 false

let try_a shift =
  Hashtbl.iter
    (fun low _ ->
      let () = Hashtbl.remove x low in
      for i = 1 to 65535 do
        let a = (i lsl shift) lor low in
        let cpu = { oi = 0; ip = 0; a; b = 0; c = 0; found = false } in
        let () = Array.fill output 0 (Array.length output) 0 in
        let cpu = loop cpu true in
        (*
        let () = if cpu.oi > 10 then
          let () = Printf.printf "%i\t" a in
          let () = binary a in
          let () = Printf.printf "\t" in
          let () = Array.iter (Printf.printf "%i,") output in
          let () = Printf.printf "\n" in
          flush stdout in
           *)
        if cpu.oi >= (shift / 4) + 4 then Hashtbl.add x a cpu.found
      done)
    (Hashtbl.copy x)

let _ = try_a 0
let _ = try_a 16
let _ = try_a 32
let () = Hashtbl.iter (fun a found -> if found then Printf.printf "%i\n" a) x
let () = Hashtbl.filter_map_inplace (fun _ found -> if found then Some true else None) x
let part2 = Hashtbl.fold (fun a _ acc -> a :: acc) x [] |> List.sort compare |> List.hd
let () = Printf.printf "part 2: %i\n" part2
