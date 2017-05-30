let almost_done stuff dist parts len = 
    let rec aux acc result parts_put random target counter= 
        match random with
        | [] -> result
        | x::xs -> if (acc+x <= target && (parts-parts_put < len-counter))  then aux (acc+x) (x::result) parts_put xs target (counter+1)
        else
            aux x (x::-1::result) (parts_put+1) xs target (counter+1)
    in 
        aux 0 [] 1 stuff dist 0
;;


let maximum inalist =
    List.fold_left
    (fun a b -> if a < b then b else a)
    (List.hd inalist)
    inalist
;;          


let  mfold shit elem = 
    match shit with 
    | (tempsum,days,target) ->
    if (tempsum + elem) > target then (elem,days+1,target) else (tempsum+elem,days,target) 


let get_snd (a,b,c) = b;;

let solve weights parts thesum themax =
    let rec bsearch first last now = 
        let mid = (first+last)/2 in
          if  first = last then 2*mid else (   
            let days = List.fold_left mfold (0,1,mid) weights in let c = (get_snd days) in
            if (c <= parts ) then bsearch first mid mid 
            else bsearch (mid +1) last mid  
          ) 
    in bsearch themax thesum (themax + thesum)/2
;;


let rec compress = function
        | a :: (b :: _ as t) -> if a = -1  then if b = a then compress t else a :: compress t else a :: compress t
        | smaller -> smaller
;;
    
let file = Sys.argv.(1);;
let ic = open_in file;;
let line1 = input_line ic;;
let parts = List.nth (List.map (int_of_string) (Str.split (Str.regexp " ") line1)) 1;;
let line2 = List.map (int_of_string) (Str.split (Str.regexp " ") (input_line ic));;
let maxtravel = solve line2 parts (List.fold_left (+) 0 line2) (maximum line2);;  
let result =  (almost_done (List.rev line2) (maxtravel) parts (List.length line2) );;   

(*
let () = (List.iter (Printf.printf "%d ") result);;  
*)
let () = (List.iter (fun x -> if (x > 0) then Printf.printf "%d " x else Printf.printf "| ") (compress result));;  
print_endline "";;
