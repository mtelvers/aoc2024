let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type coord = {
  x : int;
  y : int;
}

type robot = {
  p : coord;
  v : coord;
}

let rec read = function
  | [] -> []
  | r :: tl ->
      let r = Scanf.sscanf r " p = %i , %i v = %i , %i " @@ fun px py vx vy -> { p = { x = px; y = py }; v = { x = vx; y = vy } } in
      r :: read tl

let robots = read input
let mx, my = if List.length robots < 100 then (11, 7) else (101, 103)

let after n =
  List.map
    (fun { p; v } ->
      let x = (p.x + (v.x * n)) mod mx in
      let x = if x < 0 then x + mx else x in
      let y = (p.y + (v.y * n)) mod my in
      let y = if y < 0 then y + my else y in
      { x; y })
    robots

let after_100 = after 100

type quadrants = {
  tl : int;
  tr : int;
  bl : int;
  br : int;
}

let count_robots =
  List.fold_left
    (fun { tl; tr; bl; br } { x; y } ->
      let mx2 = mx / 2 in
      let my2 = my / 2 in
      {
        tl = (tl + if x < mx2 && y < my2 then 1 else 0);
        tr = (tr + if x > mx2 && y < my2 then 1 else 0);
        bl = (bl + if x < mx2 && y > my2 then 1 else 0);
        br = (br + if x > mx2 && y > my2 then 1 else 0);
      })
    { tl = 0; tr = 0; bl = 0; br = 0 } after_100

let part1 = count_robots.tl * count_robots.tr * count_robots.bl * count_robots.br
let () = Printf.printf "part 1: %i\n" part1

module CoordSet = Set.Make (struct
  type t = coord

  let compare = compare
end)

let rec fill pict { x; y } =
  [ { y = 0; x = 1 }; { y = 1; x = 0 }; { y = 0; x = -1 }; { y = -1; x = 0 } ]
  |> List.fold_left
       (fun pict d ->
         let p = { x = x + d.x; y = y + d.y } in
         match CoordSet.find_opt p pict with
         | Some _ -> fill pict p
         | None -> pict)
       (CoordSet.remove { x; y } pict)

let rec loop n =
  let picture = after n |> CoordSet.of_list in
  let region = fill picture { x = mx / 2; y = my / 2 } in
  let size = CoordSet.cardinal picture - CoordSet.cardinal region in
  if size < 100 then loop (n + 1) else n

let part2 = loop 1
let () = Printf.printf "part 2: %i\n" part2

let () =
  let robots = after part2 in
  for y = 0 to my do
    for x = 0 to mx do
      match List.find_opt (fun p -> p = { x; y }) robots with
      | Some _ -> Printf.printf "*"
      | None -> Printf.printf " "
    done;
    Printf.printf "\n"
  done
