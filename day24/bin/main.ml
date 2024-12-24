let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic
let inputs, gates = List.partition (fun line -> String.contains line ':') input

type logic =
  | XOR
  | OR
  | AND
  | OUT

let gate_of_string = function
  | "XOR" -> XOR
  | "OR" -> OR
  | "AND" -> AND
  | _ -> OUT

type gate = {
  in1 : string;
  g : logic;
  in2 : string;
  out : int option;
}

module StringMap = Map.Make (String)

let inputs =
  List.map (fun line -> Scanf.sscanf line " %s@: %i " @@ fun name out -> (name, { in1 = ""; g = OUT; in2 = ""; out = Some out })) inputs |> StringMap.of_list

let gates =
  List.map (fun line -> Scanf.sscanf line " %s %s %s -> %s " @@ fun in1 g in2 name -> (name, { in1; g = gate_of_string g; in2; out = None })) gates
  |> StringMap.of_list

let system = StringMap.union (fun _ -> assert false) inputs gates

let print graph =
  let oc = open_out "graph.gv" in
  let () = Printf.fprintf oc "digraph g1 {\n" in
  let () = Printf.fprintf oc "  layout=\"fdp\";\n" in
  let () = Printf.fprintf oc "  overlap=\"false\";\n" in
  let () = Printf.fprintf oc "  splines=\"true\";\n" in
  let () =
    StringMap.iter
      (fun name { in1; g; in2; _ } ->
        match g with
        | OR -> Printf.fprintf oc "  %s [shape=invtriangle]\n  %s -> %s\n  %s -> %s\n" name in1 name in2 name
        | XOR -> Printf.fprintf oc "  %s [shape=triangle]\n  %s -> %s\n  %s -> %s\n" name in1 name in2 name
        | AND -> Printf.fprintf oc "  %s [shape=diamond]\n  %s -> %s\n  %s -> %s\n" name in1 name in2 name
        | _ -> ())
      graph
  in
  let () = Printf.fprintf oc "}\n" in
  let () = close_out oc in
  Sys.command (Filename.quote_command "dot" [ "-Tpdf"; "graph.gv" ] ~stdout:"graph.pdf")

let _ = print system

let rec loop sys =
  let sys =
    StringMap.map
      (fun gate ->
        if gate.g = OUT then gate
        else
          let in1 = StringMap.find gate.in1 sys in
          let in2 = StringMap.find gate.in2 sys in
          match (in1.out, in2.out) with
          | Some in1, Some in2 -> (
              match gate.g with
              | OR -> { gate with out = Some (in1 lor in2) }
              | XOR -> { gate with out = Some (in1 lxor in2) }
              | AND -> { gate with out = Some (in1 land in2) }
              | OUT -> assert false)
          | _ -> gate)
      sys
  in
  let undefined = StringMap.fold (fun _ gate acc -> if gate.out = None then acc + 1 else acc) sys 0 in
  let () = Printf.printf "undefined = %i\n" undefined in
  if undefined > 0 then loop sys else sys

let system = loop system

let part1 = StringMap.fold (fun name gate acc -> if String.starts_with ~prefix:"z" name
                         then let () = Printf.printf "%s %i\n" name (Option.value ~default:2 gate.out) in
                           let bit = String.sub name 1 2 |> int_of_string in
                           acc lor ((Option.value ~default:0 gate.out) lsl bit)
                         else acc) system 0
let () = Printf.printf "part 1: %i\n" part1
