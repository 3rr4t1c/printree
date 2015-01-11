(* Stampa di alberi nari con rappresentazione grafica. *)

type 'a ntree = Tr of 'a * 'a ntree list

(* n-alberi di test *)

let t1 = Tr("1",
	    [Tr("2",
		[Tr("3",[]);Tr("28",[]);Tr("55",[])]);
	     Tr("6",
		[Tr("193",[]);Tr("256",[]);Tr("71",[])]);
	     Tr("557",
		[Tr("40",[]);Tr("8",[]);Tr("11",[])])]) 

let t2 = Tr("101",
	    [Tr("24",[]);
	     Tr("3",[Tr("47",[])])])

let t3 = Tr("xx",
	    [Tr("xxx",
		[Tr("xxx",[]);Tr("xxx",[])]);
	     Tr("xxx",
		[Tr("xxx",[]);Tr("xxx",[])])])

(* get_level: int -> 'a ntree -> 'a list riporta il livello di nodi n-esimo *)

let rec get_level l (Tr(x,tlist)) =
  if l <= 0 || tlist = [] then [x]
  else List.flatten (List.map (get_level (l-1)) tlist)

(* branching_factor *)

let rec branching_factor (Tr(_,tl)) =
  List.fold_right max (List.map branching_factor tl) (List.length tl)

(* height: 'a ntree -> int, height ntree = altezza di ntree. *)

let rec height (Tr(_,tl)) =
  List.fold_right max (List.map (fun t -> height t + 1) tl) 0

(* tree_stuff data una lista di sottoalberi tlist la riempie con alberi
   con radice " " e 0 figli fino a raggiungiere un numero totale target. *)

let rec tree_stuff target tlist =
  if List.length tlist >= target then tlist
  else tree_stuff target ((Tr(" ",[]))::tlist)

(* stuff_tree: string ntree -> string ntree riempie con spazi i nodi mancanti *)

let stuff_tree ntree =
  let bf = branching_factor ntree in
  let rec aux n (Tr(x,tl)) =
    if n <= 0 then (Tr(x,tl))
    else aux (n-1) (Tr(x, List.map (aux (n-1)) (tree_stuff bf tl))) in
  aux (height ntree) ntree

(* pow *)

let rec pow x y = if y = 0 then 1 else x * (pow x (y-1))

(* string_max_length: string ntree -> int lunghezza della stringa più lunga. *)

let rec string_max_length (Tr(x,tl)) =
  List.fold_right max (List.map string_max_length tl) (String.length x)

(* Conta il numero di foglie di un albero. *)

let rec conta_foglie (Tr(_,tl)) = match tl with
    [] -> 1
  | _ -> List.fold_left (+) 0 (List.map conta_foglie tl)

(* print_level: int -> string ntree -> string dato un livello da stampare di un
   albero ntree riporta la stringa che rappresenta quel livello correttamente
   indentata. *)

let print_level l ntree =
  let nfoglie = pow (branching_factor ntree) ((height ntree)-l) in
  let sml = string_max_length ntree in
  let half_spaces = (((nfoglie*sml)+(nfoglie-1))-sml)/2 in
  let indent = function
      x -> (String.make half_spaces ' ') ^ x 
	   ^ (String.make half_spaces ' ') in
  String.concat " " (List.map indent (get_level l ntree)) 

let _ = print_string begin 
	    (print_level 0 t1) ^ ("\n\n") ^
	      (print_level 1 t1) ^ ("\n\n") ^ 
		(print_level 2 t1) ^ ("\n\n") end

let _ = print_string begin 
	    (print_level 0 t3) ^ ("\n\n") ^
	      (print_level 1 t3) ^ ("\n\n") ^ 
		(print_level 2 t3) ^ ("\n\n") end

let t4 = stuff_tree t2
let _ = print_string begin 
	    (print_level 0 t4) ^ ("\n\n") ^
	      (print_level 1 t4) ^ ("\n\n") ^ 
		(print_level 2 t4) ^ ("\n\n") end

(* fill_string: int -> string -> string, dato un intero l ed una stringa s riporta
   la stessa stringa aggiungendo spazi fino a raggiungere la lunghezza l. *)

let rec fill_string l s =
  if String.length s = l then s
  else fill_string l ("x" ^ s)

(* fill_strings: string ntree -> string ntree, porta tutte le stringhe alla stessa
   lunghezza. *)

let fill_strings ntree = 
  let rec aux sml (Tr(x,tl)) = match tl with
      [] -> Tr(fill_string sml x,[])
    | _ -> Tr(fill_string sml x,(List.map (aux sml) tl)) in
  aux (string_max_length ntree) ntree

(* printree: string ntree -> unit, dato un n-albero di stringhe lo stampa
   con la rappresentazione grafica. *) 

let printree ntree = 
  let h = height ntree in
  let rec aux lev =
    if lev > h then ""
    else (print_level lev (fill_strings (stuff_tree ntree))) ^ 
	   ("\n\n") ^ aux (lev+1) in
  print_string (aux 0)

let _ = print_string begin 
	    (print_level 0 (fill_strings (stuff_tree t2))) ^ ("\n\n") ^
	      (print_level 1 (fill_strings (stuff_tree t2))) ^ ("\n\n") ^ 
		(print_level 2 (fill_strings (stuff_tree t2))) ^ ("\n\n") end
