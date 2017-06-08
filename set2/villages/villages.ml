(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Persistent union-find = Tarjan's algorithm with persistent arrays *)

(* persistent arrays; see the module [Parray] for explanations *)
module Pa = struct

  type t = data ref
  and data =
    | Array of int array 
    | Diff of int * int * t
        
  let create n v = ref (Array (Array.make n v))
  let init n f = ref (Array (Array.init n f))
    
  (* reroot t ensures that t becomes an Array node *)
  let rec reroot t = match !t with
    | Array _ -> ()
    | Diff (i, v, t') -> 
        reroot t';
        begin match !t' with
          | Array a as n ->
              let v' = a.(i) in
              a.(i) <- v;
              t := n;
              t' := Diff (i, v', t)
          | Diff _ -> assert false
        end
  
  let rec rerootk t k = match !t with
    | Array _ -> k ()
    | Diff (i, v, t') -> 
        rerootk t' (fun () -> begin match !t' with
                      | Array a as n ->
                          let v' = a.(i) in
                          a.(i) <- v;
                          t := n;
                          t' := Diff (i, v', t)
                      | Diff _ -> assert false end; k())

  let reroot t = rerootk t (fun () -> ())

  let rec get t i = match !t with
    | Array a -> 
        a.(i)
    | Diff _ -> 
        reroot t; 
        begin match !t with Array a -> a.(i) | Diff _ -> assert false end
      
  let set t i v = 
    reroot t;
    match !t with
      | Array a as n ->
          let old = a.(i) in
          if old == v then
            t
          else begin
            a.(i) <- v;
            let res = ref n in
            t := Diff (i, old, res);
            res
          end
      | Diff _ ->
          assert false

end

(* Tarjan's algorithm *)

type t = { 
  mutable father: Pa.t; (* mutable to allow path compression *)
  c: Pa.t; (* ranks *)
  comps: int;
}
      
let create n = 
  { c = Pa.create n 0;
    father = Pa.init n (fun i -> i);
    comps = n;
  }
    
let rec find_aux f i = 
  let fi = Pa.get f i in
  if fi == i then 
    f, i
  else 
    let f, r = find_aux f fi in 
    let f = Pa.set f i r in
    f, r
      
let find h x = 
  let f,rx = find_aux h.father x in h.father <- f; rx
  
let union h x y = 
  let rx = find h x in
  let ry = find h y in
  if rx != ry then begin
    let rxc = Pa.get h.c rx in
    let ryc = Pa.get h.c ry in
    if rxc > ryc then
        { h with father = Pa.set h.father ry rx;
          comps = (h.comps-1)}
    else if rxc < ryc then
        { h with father = Pa.set h.father rx ry;
          comps = (h.comps-1)}
    else
      { c = Pa.set h.c rx (rxc + 1);
        father = Pa.set h.father ry rx;
        comps = (h.comps-1);}
  end else
    h


let solve n m ic  = 
    let t = create n in 
    let rec loop t i = 
    if ( i >= m ) then t.comps else
    (let [x;y] = List.map int_of_string (Str.split (Str.regexp " ") (input_line ic)) in
    let t = union t (x-1) (y-1) in
    loop t (i+1)
    )
in loop t 0

let file = Sys.argv.(1);;
let ic = open_in file;;
let [n;m;c] = List.map int_of_string (Str.split (Str.regexp_string " ") (input_line ic));;
Printf.printf "%d\n" ((solve n m ic)-c);;
