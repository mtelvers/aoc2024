let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type cpu = {
  ip : int;
  a : int;
  b : int;
  c : int;
}

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

let registers, memory = input |> List.filter (fun l -> String.length l > 0) |> List.partition (fun l -> String.split_on_char ' ' l |> List.length > 2)
let registers = String.concat ", " registers
let cpu = Scanf.sscanf registers " Register A: %i , Register B: %i , Register C: %i " @@ fun a b c -> { ip = 0; a; b; c }
let memory = String.concat "" memory
let memory = Scanf.sscanf memory " Program: %s " @@ fun p -> p
let memory = String.split_on_char ',' memory |> List.map int_of_string |> Array.of_list

let operand cpu = function
  | 0 -> 0
  | 1 -> 1
  | 2 -> 2
  | 3 -> 3
  | 4 -> cpu.a
  | 5 -> cpu.b
  | 6 -> cpu.c
  | _ -> assert false

let rec loop cpu =
(* let () = Printf.printf "IP %i A %i B %i C %i\n" cpu.ip cpu.a cpu.b cpu.c in *)
  if cpu.ip = Array.length memory then
    cpu
  else
  match Array.get memory cpu.ip with
  | 0 (* adv *) ->
      let op = operand cpu (Array.get memory (cpu.ip + 1)) in
      loop { cpu with ip = cpu.ip + 2; a = cpu.a / (pow 2 op) }
  | 1 (* bxl *) ->
      let op = Array.get memory (cpu.ip + 1) in
      loop { cpu with ip = cpu.ip + 2; b = cpu.b lxor op }
  | 2 (* bst *) ->
      let op = operand cpu (Array.get memory (cpu.ip + 1)) in
      loop { cpu with ip = cpu.ip + 2; b = op mod 8 }
  | 3 (* jnz *) ->
      let op = Array.get memory (cpu.ip + 1) in
      if cpu.a = 0 then loop { cpu with ip = cpu.ip + 2 } else loop { cpu with ip = op }
  | 4 (* bxc *) -> loop { cpu with ip = cpu.ip + 2; b = cpu.b lxor cpu.c }
  | 5 (* out *) ->
      let op = operand cpu (Array.get memory (cpu.ip + 1)) in
      let () = Printf.printf "%i," (op mod 8) in
      loop { cpu with ip = cpu.ip + 2 }
  | 6 (* bdv *) ->
      let op = operand cpu (Array.get memory (cpu.ip + 1)) in
      loop { cpu with ip = cpu.ip + 2; b = cpu.a / (pow 2 op) }
  | 7 (* cdv *) ->
      let op = operand cpu (Array.get memory (cpu.ip + 1)) in
      loop { cpu with ip = cpu.ip + 2; c = cpu.a / (pow 2 op) }
  | _ -> assert false

let () = Printf.printf "part 1: "
let _ = loop cpu
let () = Printf.printf "\n"
