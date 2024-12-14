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
      { p = { x; y }; v })
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
    (fun { tl; tr; bl; br } { p = { x; y }; _ } ->
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
