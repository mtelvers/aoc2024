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

let string_of_gate = function
  | XOR -> "XOR"
  | OR -> "OR"
  | AND -> "AND"
  | OUT -> "OUT"

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

let print name graph =
  let oc = open_out (name ^ ".gv") in
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
  Sys.command (Filename.quote_command "dot" [ "-Tpdf"; name ^ ".gv" ] ~stdout:(name ^ ".pdf"))

(*
let _ = print system
*)

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

let part1 =
  StringMap.fold
    (fun name gate acc ->
      if String.starts_with ~prefix:"z" name then
        let () = Printf.printf "%s %i\n" name (Option.value ~default:2 gate.out) in
        let bit = String.sub name 1 2 |> int_of_string in
        acc lor (Option.value ~default:0 gate.out lsl bit)
      else acc)
    system 0

let () = Printf.printf "part 1: %i\n" part1

(*
x =  101111010011010011110101011111101111011001101
y =  110111101110001111001111001111010100000011001
z = 1100110111111100011000100101110111011101100110 (wrong)
z = 1100111000001100011000100101111000011011100110 (actual)
    1100110111111100011000100101110111011011100110
          XXXXXX                  XXXX   XX       

x = 26004394335949
y = 30633751980057
z = 56638146316006 (actual)
   *)

let rec find n =
  let gate = StringMap.find n system in
  match gate.g with
  | OUT -> [ (n, gate) ]
  | _ -> (n, gate) :: (find gate.in1 @ find gate.in2)

let _ =
  List.fold_left
    (fun acc name ->
      let () = Printf.printf "%s " name in
      let zNN = find name |> List.sort_uniq compare in
      let extra = List.filter (fun (n, _) -> not (StringMap.mem n acc)) zNN |> StringMap.of_list in
      let () = Printf.printf "(%i) " (List.length zNN) in
      let lst =
        StringMap.fold
          (fun n gate acc ->
            (match gate.g with
            | OUT -> Printf.sprintf "%s, " n
            | _ -> Printf.sprintf "%s %s %s = %s, " (string_of_gate gate.g) gate.in1 gate.in2 n)
            :: acc)
          extra []
      in
      let lst = List.sort compare lst in
      let () = List.iter print_string lst in
      let () = Printf.printf "\n" in
      let () = flush stdout in
      let _ = print name extra in
      zNN |> StringMap.of_list)
    StringMap.empty
    [ "z00"; "z01"; "z02"; "z03"; "z04"; "z05"; "z06"; "z07";
      "z08"; "z09"; "z10"; "z11"; "z12"; "z13"; "z14"; "z15";
      "z16"; "z17"; "z18"; "z19"; "z20"; "z21"; "z22"; "z23";
      "z24"; "z25"; "z26"; "z27"; "z28"; "z29"; "z30"; "z31";
      "z32"; "z33"; "z34"; "z35"; "z36"; "z37"; "z38"; "z39";
      "z40"; "z41"; "z42"; "z43"; "z44"; "z45"; ]

(*

z00 (3) XOR x00 y00 = z00, x00, y00,
z01 (7) AND y00 x00 = whb, XOR jjd whb = z01, XOR x01 y01 = jjd, x01, y01,
z02 (13) AND jjd whb = wbw, AND x01 y01 = bdf, OR bdf wbw = qkf, XOR qkf wsv = z02, XOR x02 y02 = wsv, x02, y02,
z03 (19) AND qkf wsv = pqc, AND x02 y02 = vdf, OR pqc vdf = bkh, XOR bcj bkh = z03, XOR y03 x03 = bcj, x03, y03,
z04 (25) AND bkh bcj = rhq, AND x03 y03 = htr, OR htr rhq = rjc, XOR cjb rjc = z04, XOR x04 y04 = cjb, x04, y04,
z05 (31) AND cjb rjc = fsb, AND x04 y04 = jjm, OR jjm fsb = nbm, XOR nbm dqp = z05, XOR x05 y05 = dqp, x05, y05,
z06 (37) AND dqp nbm = hvv, AND y05 x05 = gtb, OR gtb hvv = gtn, XOR gtn qmt = z06, XOR x06 y06 = qmt, x06, y06,
z07 (45) AND cds rkv = nph, AND gtn qmt = jmk, AND x06 y06 = ssv, AND x07 y07 = sdj, OR jmk ssv = rkv, OR sdj nph = z07, XOR x07 y07 = cds, x07, y07,
z08 (47) XOR cds rkv = rts, XOR hmv rts = z08, XOR x08 y08 = hmv, x08, y08,
z09 (53) AND rts hmv = ptf, AND x08 y08 = jrr, OR ptf jrr = sqm, XOR mtg sqm = z09, XOR y09 x09 = mtg, x09, y09,
z10 (59) AND mtg sqm = chg, AND x09 y09 = nvw, OR nvw chg = vgp, XOR vpc vgp = z10, XOR y10 x10 = vpc, x10, y10,
z11 (65) AND vgp vpc = qht, AND x10 y10 = bfm, OR qht bfm = htq, XOR rtk htq = z11, XOR y11 x11 = rtk, x11, y11,
z12 (71) AND htq rtk = dsm, AND ksn nft = z12, AND y11 x11 = smr, OR smr dsm = ksn, XOR x12 y12 = nft, x12, y12,
z13 (77) AND y12 x12 = mqn, OR mqn jpj = bwg, XOR bwg mfn = z13, XOR ksn nft = jpj, XOR y13 x13 = mfn, x13, y13,
z14 (83) AND bwg mfn = djp, AND x13 y13 = pfb, OR djp pfb = qvr, XOR qvr fnq = z14, XOR y14 x14 = fnq, x14, y14,
z15 (89) AND qvr fnq = mch, AND y14 x14 = tfr, OR mch tfr = ppb, XOR ppb hmr = z15, XOR y15 x15 = hmr, x15, y15,
z16 (95) AND ppb hmr = hqd, AND x15 y15 = fpr, OR hqd fpr = hgh, XOR svs hgh = z16, XOR y16 x16 = svs, x16, y16,
z17 (101) AND svs hgh = rff, AND x16 y16 = mts, OR rff mts = rpg, XOR ffj rpg = z17, XOR x17 y17 = ffj, x17, y17,
z18 (107) AND ffj rpg = bfk, AND y17 x17 = jvm, OR bfk jvm = gwb, XOR kvf gwb = z18, XOR x18 y18 = kvf, x18, y18,
z19 (113) AND gwb kvf = pkd, AND y18 x18 = jkm, OR jkm pkd = rjf, XOR bqh rjf = z19, XOR y19 x19 = bqh, x19, y19,
z20 (119) AND rjf bqh = cmf, AND x19 y19 = wqs, OR wqs cmf = tpf, XOR tpf vhb = z20, XOR x20 y20 = vhb, x20, y20,
z21 (125) AND tpf vhb = tpp, AND x20 y20 = pfj, OR tpp pfj = twv, XOR qcm twv = z21, XOR x21 y21 = qcm, x21, y21,
z22 (131) AND qcm twv = jth, AND x21 y21 = cgd, OR cgd jth = dqj, XOR wgq dqj = z22, XOR y22 x22 = wgq, x22, y22,
z23 (137) AND wgq dqj = tws, AND x22 y22 = prp, OR tws prp = hbw, XOR vfm hbw = z23, XOR y23 x23 = vfm, x23, y23,
z24 (143) AND hbw vfm = kwg, AND x23 y23 = vjr, OR vjr kwg = pkr, XOR kcv pkr = z24, XOR x24 y24 = kcv, x24, y24,
z25 (149) AND pkr kcv = thc, AND x24 y24 = sqt, OR thc sqt = rbd, XOR rbd knm = z25, XOR x25 y25 = knm, x25, y25,
z26 (3) AND x26 y26 = z26, x26, y26,
z27 (161) AND bkh bcj = rhq, AND bvp gdb = stc, AND bwg mfn = djp, AND cjb rjc = fsb, AND dqp nbm = hvv, AND ffj rpg = bfk, AND gtn qmt = jmk, AND gwb kvf = pkd, AND hbw vfm = kwg, AND htq rtk = dsm, AND jjd whb = wbw, AND mtg sqm = chg, AND pkr kcv = thc, AND ppb hmr = hqd, AND qcm twv = jth, AND qkf wsv = pqc, AND qvr fnq = mch, AND rbd knm = rdj, AND rjf bqh = cmf, AND rts hmv = ptf, AND svs hgh = rff, AND tpf vhb = tpp, AND vgp vpc = qht, AND wgq dqj = tws, AND x01 y01 = bdf, AND x02 y02 = vdf, AND x03 y03 = htr, AND x04 y04 = jjm, AND x06 y06 = ssv, AND x08 y08 = jrr, AND x09 y09 = nvw, AND x10 y10 = bfm, AND x13 y13 = pfb, AND x15 y15 = fpr, AND x16 y16 = mts, AND x19 y19 = wqs, AND x20 y20 = pfj, AND x21 y21 = cgd, AND x22 y22 = prp, AND x23 y23 = vjr, AND x24 y24 = sqt, AND y00 x00 = whb, AND y05 x05 = gtb, AND y11 x11 = smr, AND y12 x12 = mqn, AND y14 x14 = tfr, AND y17 x17 = jvm, AND y18 x18 = jkm, AND y25 x25 = kfv, OR bdf wbw = qkf, OR bfk jvm = gwb, OR cgd jth = dqj, OR djp pfb = qvr, OR gtb hvv = gtn, OR hqd fpr = hgh, OR htr rhq = rjc, OR jjm fsb = nbm, OR jkm pkd = rjf, OR jmk ssv = rkv, OR kgj stc = www, OR mch tfr = ppb, OR mqn jpj = bwg, OR nvw chg = vgp, OR pqc vdf = bkh, OR ptf jrr = sqm, OR qht bfm = htq, OR rdj kfv = gdb, OR rff mts = rpg, OR smr dsm = ksn, OR thc sqt = rbd, OR tpp pfj = twv, OR tws prp = hbw, OR vjr kwg = pkr, OR wqs cmf = tpf, XOR bvp gdb = kgj, XOR cds rkv = rts, XOR ksn nft = jpj, XOR www qdf = z27, XOR x01 y01 = jjd, XOR x02 y02 = wsv, XOR x04 y04 = cjb, XOR x05 y05 = dqp, XOR x06 y06 = qmt, XOR x07 y07 = cds, XOR x08 y08 = hmv, XOR x12 y12 = nft, XOR x17 y17 = ffj, XOR x18 y18 = kvf, XOR x20 y20 = vhb, XOR x21 y21 = qcm, XOR x24 y24 = kcv, XOR x25 y25 = knm, XOR x26 y26 = bvp, XOR y03 x03 = bcj, XOR y09 x09 = mtg, XOR y10 x10 = vpc, XOR y11 x11 = rtk, XOR y13 x13 = mfn, XOR y14 x14 = fnq, XOR y15 x15 = hmr, XOR y16 x16 = svs, XOR y19 x19 = bqh, XOR y22 x22 = wgq, XOR y23 x23 = vfm, XOR y27 x27 = qdf, x00, x01, x02, x03, x04, x05, x06, x07, x08, x09, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x27, y00, y01, y02, y03, y04, y05, y06, y07, y08, y09, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23, y24, y25, y27,
z28 (167) AND www qdf = vjf, AND y27 x27 = wth, OR wth vjf = btv, XOR btv cfj = z28, XOR x28 y28 = cfj, x28, y28,
z29 (173) AND btv cfj = tdv, AND y28 x28 = wjp, OR tdv wjp = cjw, XOR ntj cjw = z29, XOR x29 y29 = ntj, x29, y29,
z30 (179) AND cjw ntj = dtj, AND x29 y29 = vvq, OR dtj vvq = jvp, XOR kbn jvp = z30, XOR y30 x30 = kbn, x30, y30,
z31 (185) AND jvp kbn = gtq, AND x30 y30 = qgf, OR gtq qgf = fmt, XOR svg fmt = z31, XOR y31 x31 = svg, x31, y31,
z32 (191) AND svg fmt = qnk, AND x31 y31 = cvn, OR cvn qnk = cjg, XOR kkd cjg = z32, XOR x32 y32 = kkd, x32, y32,
z33 (197) AND kkd cjg = mdv, AND x32 y32 = rft, OR mdv rft = rnq, XOR chc rnq = z33, XOR x33 y33 = chc, x33, y33,
z34 (203) AND chc rnq = vjh, AND y33 x33 = mmb, AND y34 x34 = chv, OR mmb vjh = fqf, XOR chv fqf = z34, x34, y34,
z35 (209) AND fqf chv = cwh, OR cwh vvw = ttb, XOR cbs ttb = z35, XOR x34 y34 = vvw, XOR x35 y35 = cbs, x35, y35,
z36 (215) AND cbs ttb = qtd, AND y35 x35 = bbh, OR bbh qtd = jfq, XOR jfq fbb = z36, XOR x36 y36 = fbb, x36, y36,
z37 (221) AND jfq fbb = trr, AND y36 x36 = kfn, OR trr kfn = hck, XOR hck cvr = z37, XOR y37 x37 = cvr, x37, y37,
z38 (227) AND cvr hck = pjd, AND y37 x37 = dsg, OR pjd dsg = mwv, XOR mwv jsg = z38, XOR y38 x38 = jsg, x38, y38,
z39 (233) AND mwv jsg = wdw, AND x38 y38 = vqt, OR wdw vqt = cmt, XOR nbq cmt = z39, XOR x39 y39 = nbq, x39, y39,
z40 (239) AND cmt nbq = gmc, AND y39 x39 = gwq, OR gwq gmc = sgt, XOR sgt scc = z40, XOR x40 y40 = scc, x40, y40,
z41 (245) AND sgt scc = bnv, AND x40 y40 = bst, OR bnv bst = stg, XOR qpp stg = z41, XOR y41 x41 = qpp, x41, y41,
z42 (251) AND stg qpp = dnf, AND x41 y41 = pkj, OR dnf pkj = ksp, XOR pvj ksp = z42, XOR x42 y42 = pvj, x42, y42,
z43 (257) AND pvj ksp = dkc, AND y42 x42 = mkg, OR mkg dkc = sch, XOR srj sch = z43, XOR y43 x43 = srj, x43, y43,
z44 (263) AND sch srj = whj, AND x43 y43 = pnn, OR whj pnn = bmp, XOR rrq bmp = z44, XOR y44 x44 = rrq, x44, y44,
z45 (265) AND rrq bmp = cbv, AND y44 x44 = gqr, OR gqr cbv = z45

Analysis by hand shows that for each zNN.pdf there should be, e.g.

   (x06 xor y06) xor ((x05 and x06) or (nbm and dqp))

Thus swaps are:

- rts,z07
- kgj,z26
- jpj,z12
- chv,vvw

chv,jpj,kgj,rts,vvw,z07,z12,z26

*)
