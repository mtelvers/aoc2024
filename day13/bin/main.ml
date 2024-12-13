let input = In_channel.with_open_text "input" @@ fun ic -> In_channel.input_lines ic

type coord = {
  x : int;
  y : int;
}

type machine = {
  a : coord;
  b : coord;
  p : coord;
}

let rec read = function
  | [] -> []
  | "" :: tl -> read tl
  | a :: b :: p :: tl ->
      let a = Scanf.sscanf a " Button A: X+%s@, Y+%s " @@ fun x y -> { x = int_of_string x; y = int_of_string y } in
      let b = Scanf.sscanf b " Button B: X+%s@, Y+%s " @@ fun x y -> { x = int_of_string x; y = int_of_string y } in
      let p = Scanf.sscanf p " Prize: X=%s@, Y=%s " @@ fun x y -> { x = int_of_string x; y = int_of_string y } in
      { a; b; p } :: read tl
  | _ :: tl -> read tl

let machines = read input

(*
 * Button A: X+94, Y+34
 * Button B: X+22, Y+67
 * Prize: X=8400, Y=5400
 * 94n + 22m = 8400
 * 34n + 67m = 5400
 *  
 * (a.x)n + (b.x)m = p.x
 * (a.y)n + (b.y)m = p.y
 *  
 * ( a.x b.x ) ( n ) = ( p.x )
 * ( a.y b.y ) ( m )   ( p.y )
 *  
 * ( n ) =  1  ( b.y  -b.x ) ( p.x )
 * ( m )   det ( -a.y a.x ) ( p.y )
 *  
 * ( n ) =  1  ( b.y * p.x + -b.x * p.y )
 * ( m )   det ( -a.y * p.x + a.x * p.y )
 *)

let part1 =
  List.fold_left
    (fun sum { a; b; p } ->
      let det = (a.x * b.y) - (b.x * a.y) in
      let n = ((b.y * p.x) + (-b.x * p.y)) / det in
      let m = ((-a.y * p.x) + (a.x * p.y)) / det in
      let cost = if m < 0 || m > 100 || n < 0 || n > 100 then 0 else (3 * n) + m in
      let () = Printf.printf "a %i,%i b %i,%i p %i,%i -> %i %i = %i\n" a.x a.y b.x b.y p.x p.y n m cost in
      let cost = if (n * a.x) + (m * b.x) = p.x && (n * a.y) + (m * b.y) = p.y then cost else 0 in
      sum + cost)
    0 machines

let () = Printf.printf "part 1: %i\n" part1

let correction = 10000000000000
let machines = List.map (fun m -> { m with p = { x = correction + m.p.x; y = correction + m.p.y }}) machines

let part2 =
  List.fold_left
    (fun sum { a; b; p } ->
      let det = (a.x * b.y) - (b.x * a.y) in
      let n = ((b.y * p.x) + (-b.x * p.y)) / det in
      let m = ((-a.y * p.x) + (a.x * p.y)) / det in
      let cost = if m < 0 || n < 0 then 0 else (3 * n) + m in
      let cost = if (n * a.x) + (m * b.x) = p.x && (n * a.y) + (m * b.y) = p.y then cost else 0 in
      let () = Printf.printf "a %i,%i b %i,%i p %i,%i -> %i %i = %i\n" a.x a.y b.x b.y p.x p.y n m cost in
      sum + cost)
    0 machines

let () = Printf.printf "part 2: %i\n" part2

