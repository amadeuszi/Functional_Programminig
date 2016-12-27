(*******************************************)
(*                                         *)
(*      Autor: Amadeusz Iwanowski          *)
(*      Code review: Adam Jedrych          *)
(*                                         *)
(*******************************************)

open PMap;;

exception Cykliczne;;

type visit = Visited | Visiting | Not_visited;;


(*
Wejscie: (a, b) list, dodaje wszystkie wartosci z listy 
do mapy i ustawia na Not_visited
*)
let add_all l =
	let rec pom acu li =
		match li with
			| [] -> acu
			| (a, b) :: t -> pom (add a (ref Not_visited, b) acu) t
	in pom empty l;;

let rec iter2 funct li =
	match li with
		| [] -> ();
		| (a, b) :: t -> funct a; iter2 funct t;;

let topol l =
	let map = ref (add_all l) in
	let acu = ref ([]) in
	let rec dfs vertice =
		if exists vertice !map then
			let (c, li) = find vertice (!map) in
			match !c with
				| Visiting -> raise Cykliczne
				| Visited -> ()
				| Not_visited ->
					begin
					c := Visiting;
					List.iter dfs li;
					c := Visited;
					acu := vertice :: (!acu);
					end
		else
			begin
			map:= add vertice (ref Visited, []) !map;
			acu := vertice :: !acu;
			end
	in iter2 dfs l; !acu;;

let lista = [("kurtka", ["szalik"; "czapka"]); ("szalik", ["czapka"]); 
             ("buty", ["kurtka"])];;

assert (topol lista = ["buty"; "kurtka"; "szalik"; "czapka"]);; (*prosty test*)

let lista = [("kurtka", ["szalik"; "czapka"]); ("szalik", ["czapka"]); 
             ("buty", ["kurtka"]); ("czapka", ["kurtka"])];;

let c = try topol lista with
	| Cykliczne -> [];;

assert (c = []); (*cykliczne*)











