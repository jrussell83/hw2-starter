let abs x =
  if x >= 0 then x
  else (-x)

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup (tup : 'a * 'b) = 
  let (a,b) = tup in (b,a)
                                
                                
let rev_triple (tup : 'a * 'b * 'c) = 
  let (a,b,c) = tup in (c,b,a)

let is_odd x = 
  if x mod 2 = 0 then false else true

let is_older (date1: int * int * int) (date2: int * int * int) = 
  let (y1, m1, d1) = date1 in
  let (y2, m2, d2) = date2 in
  if y1 < y2 then true 
  else if y1 > y2 then false 
  else if m1 < m2 then true
  else if m1 > m2 then false
  else d1 < d2

let to_us_format (date1: int * int * int) = 
  let (y, m, d) = date1 in (m, d, y)
(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec pow x p =
  if p = 0 then 1 else x * pow x (p - 1)


let rec fac n = 
  if n = 1 then 1 else n * fac (n - 1)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth ((idx:int), (lst: 'a list)) = 
  match lst with
  | [] -> failwith "Index out of bounds"
  | i :: v -> if idx = 0 then i else get_nth (idx - 1, v)

let larger lst1 lst2 = 
  let i = List.length lst1 in
  let v = List.length lst2 in
  if i > v then lst1
  else if v > i then lst2
  else []

let sum lst1 lst2 = 
  let rec total lst =
    match lst with
    | [] -> 0
    | h :: t -> h + total t
  in total lst1 + total lst2



