let rec solve2 (l1,l2) = 
    let rec solve (l1, l2) i j v = 
        match  (l1, l2) with
        |(a::s, b::t) -> if (a<=b) then solve (a::s,t) i (j+1) (max v (j-i)) else solve (s,b::t) (i+1) j v
        | (x,[]) -> v
        | ([],y) -> v

    in solve (l1,l2) 1 1 0

let lmin e l = 
    let rec lmin2 e l acc =
        match l with
        | a :: t ->  (lmin2 (min a e) t ((min a e)::acc))
        | a -> List.rev acc
    in lmin2 e l []

let rmax e l  = 
    let rec rmax2 e l acc =
        match l with
        | a :: t ->  (rmax2  (max a e) t ((max a e)::acc))
        | a ->   acc
    in rmax2 e l []


let smap f list =
  let rec loop acc = function
    | [] -> List.rev acc
    | x::xs -> loop (f x::acc) xs in
  loop [] list


let file = Sys.argv.(1);;
let ic = open_in file;;
let line2 = input_line ic;;
let line2 = smap int_of_string (Str.split (Str.regexp " ") (input_line ic));;
let l1 = (lmin (List.hd line2) line2 );;
let l2 =  (rmax (List.hd (List.rev line2)) (List.rev line2) );;
let sol = solve2 (l1,l2) in
print_endline (string_of_int sol);;
