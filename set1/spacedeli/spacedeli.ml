module Indy =
    struct 
    type t = (int*int*int*int)
    let compare (x1,y1,z1,c) (x2,y2,z2,c1) = 
    let res = compare c c1 in
        if res = 0 then
            (*if x1<x2 || y1<y2 || z1<z2 then -1
            else 0
            *)
            compare (x1,y1,z1) (x2,y2,z2)
        else res
end

    module IntSet = Set.Make(Indy) ;;


let read_file filename =
    let lines = ref [] in
    let chan = open_in filename in
    try
          while true; do
                  lines := input_line chan :: !lines
                    done; !lines
    with End_of_file ->
          close_in chan;
            List.rev !lines ;;


    (*
let printttttt_sol hash dest = 
    let rec bt path counter node level =
        match Hashtbl.find node with
        | 'S' -> List.rev path
        | (x,y,d) -> if d = 'W' then bt 'W'::path (x,y,1-level) (1-level)
                     else bt d::path (x,y,level) level

    in bt [] dest 0
    *)

let lel (_,y,_) = y
let thrd (_,_,x) = x
let ha (_,_,_,c) = c
let sks (x,_,_) = x

let print_bool a = 
    match a with
    | true -> print_int 1
    | false -> print_int 0
let print_bool2 a = 
    match a with
    | true -> print_string "NAIIIIIIIIIIII\n"
    | false -> print_string "nooooo\n"


let print_me ll = 
    List.iter (fun (x,y,z,c) -> Printf.printf "%d %d %d %d\n" x y z c ) ll
let debug x y z c = Printf.printf "POPPED x:%d y:%d z:%d cost:%d\n" x y z c
let debug2 x y z c = Printf.printf "Inside insert x:%d y:%d z:%d cost:%d\n" x y z c
let debug3 (x, y, z, c) = Printf.printf "After pop In set: x:%d y:%d z:%d cost:%d\n" x y z c
let debug4 (x, y, z,c) = Printf.printf "Processing node: x:%d y:%d z:%d cost:%d\n" x y z c
let debug5 (x, y, z, c) = Printf.printf "Element: x:%d y:%d z:%d cost:%d\n" x y z c
let debug10 (x, y, z, c) = Printf.printf "After iter In set: x:%d y:%d z:%d cost:%d\n\n" x y z c


let find_sol (x,y,z,c) weed source= 
                  
    let rec calc_path path (x,y,z) = 
    let (a,b,c,e) = Hashtbl.find weed (x,y,z) in
    if  (a,b,z) = source then c::path else (
    match (a,b,c,e) with
    | (x1,y1,'W',e) -> (*let () = Printf.printf "%d %d %d W\n"  x1 y1 (1-z) in*) calc_path ('W'::path) (x1,y1,1-z)
    | (x1,y1,c,e) -> (*let () = Printf.printf "%d %d %d %c\n"  x1 y1 z c in *) calc_path (c::path) (x1,y1,z)
    (*| (x1,y1,c) -> print_string "GEIAAAAAA" ;calc_path (c::path) (x1,y1,z) *)
    )
    in
        calc_path [] (x,y,z)

let dijkstra source grid n m = 
    let set = IntSet.empty in
    let vs = Hashtbl.create 100 in
    let () = Hashtbl.add vs source (0,0,'S',0) in
    let (j,k,l) = source in
    let set = IntSet.add (j,k,l,0) set in
    let rec iteration set  = 
        if IntSet.is_empty set then  (-1,[])
        else (
        let node = IntSet.min_elt set in
        let (x,y,z,c) = node in
        let set = IntSet.remove node set in
        (*let () = debug4 node in  *)
        (*let () = IntSet.iter debug3 set in *)
        if grid.(x).[y] = 'E' && z = 1 then let solution = find_sol (x,y,1,c) vs source in (c,solution)
        (*if (x,y,z) = dest then print_sol c *)
		else 
            (
            let potential = [ (x+1,y,z,'D'); (x-1,y,z,'U'); (x,y+1,z,'R'); (x,y-1,z,'L'); (x,y,1-z,'W')] in
            let find_cost (x1,y1,z1,d) = 

                            if d = 'W' then 
                                          (c+1) 
							else (
                                match z1 with
                                | 0 ->  
                                        (c+1) 
                                | 1 ->  
                                         (c+2) 
                                | _ -> -1
                            )                    
        in 
         let neighbours = 
                List.filter 
                (   fun (x1,y1,z1,d) ->
                    if (Hashtbl.mem vs (x1,y1,z1)) && (ha (Hashtbl.find vs (x1,y1,z1))) <= find_cost (x1,y1,z1,d)   
                    then false
                    else if (x1 < 0 || x1 > n || y1 < 0 || y1 > m)
                    then false
                    else if grid.(x1).[y1] = 'X' 
                    then false 
                    else if d='W' then if grid.(x1).[y1]='W' then true else false
                    else true
                )   
                potential (*in let a = 
                List.hd neighbours
                *)
                

         in  let insert (x1,y1,z1,d) =
							let new_state = (x1,y1,z1) in
                            if d = 'W' then 
                                    let () = Hashtbl.add vs new_state (x,y,d,c+1) in
                                         (x1, y1, z1, (c+1)) 
							else (
                                match z1 with
                                | 0 ->  
                                    let () = Hashtbl.add vs new_state (x,y,d,c+1) in
                                        (x1, y1, z1, (c+1)) 
                                | 1 ->  
                                    let () = Hashtbl.add vs new_state (x,y,d,c+2) in
                                        (x1, y1, z1, (c+2)) 
                                | _ -> (-1,-1,-1,-1)
                            )                    
        in 
        match neighbours with 
        | [] -> iteration set
        | _ -> (
        let toinsert = List.map insert neighbours in
        (*let () = List.iter debug5 toinsert in *)
        let set = List.fold_right IntSet.add toinsert set in
        (*let () = IntSet.iter debug10 set in*)
         iteration set 
        )
            )
    )
    in
    iteration set



let read_file filename =
    let lines = ref [] in
    let chan = open_in filename in
    try
          while true; do
                  lines := input_line chan :: !lines
                    done; !lines
    with End_of_file ->
          close_in chan;
            List.rev !lines ;;

let find_start grid =
    let rec process_row i =
        if String.contains grid.(i) 'S' then (i,String.index grid.(i) 'S')
        else process_row (i+1)
    in process_row 0


let file = Sys.argv.(1);;
let ic = open_in file;;
let lists = read_file file;;
let grid = Array.of_list lists;;
let (x,y) = find_start grid;;
let (c,path) = dijkstra (x,y,1)  grid  (Array.length grid-1) (String.length grid.(0)-1) in
print_int c;print_string " ";List.iter (Printf.printf "%c") path;print_endline "";;



                   (*
let () = 
    let grid = 
        [|
            [|'S'; '.'; '.'; '.'|];
            [|'.'; '.'; '.'; '.'|];
            [|'W'; '.'; '.'; 'W'|];
            [|'.'; '.'; 'X'; 'E'|];
        |]
    in 
    let source = (0,0,1) in
    let dest = (3,3,1) in
    let (cost,path) = dijkstra source dest grid
    in print_int cost;List.iter (Printf.printf "%c") path
;;
*)

