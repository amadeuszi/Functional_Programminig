(********************************************)
(*                                          *)
(*      Autor: Amadeusz Iwanowski           *)
(*      Code review: Adrian Akerman         *)
(*                                          *)
(********************************************)

open List;;

type point = float * float;;

type kartka = point -> int;;

let epsilon = 0.000000000001;;
let (=.) a b = (a -. b < epsilon && a -. b > -.epsilon);;

let prostokat ((x1, y1): point) ((x2, y2): point) = 
	(
		(
			fun (x, y) -> 
				if x >= x1 && x <= x2 && y >= y1 && y <= y2 then 
					1 
				else 
					0
		): kartka
	);;



let iloczyn_skalarny (x, y) (x1, y1) (x2, y2) =
	(x -. x1) *. (x2 -. x1) +. (y -. y1) *. (y2 -. y1);;


(*Rzut punktu a na prosta zadana przez punkty b c*)
let rzut a b c =
	let z = iloczyn_skalarny a b c /. iloczyn_skalarny c b c in
		match a, b, c with
			|(x, y), (x1, y1), (x2, y2) -> (x1 +. z *. (x2 -. x1), y1 +. z *. (y2 -. y1));;


(*zwraca punkt symetryczny do punktu (x, y) wzgledem prostej zadanej przez dwa punkty*)
let symetria (x, y) (x1, y1) (x2, y2) = 
	if (x1 =. x2) then
		((x1 -. x) +. x1, y)
	else if y1 =. y2 then
		(x, (y1 -. y) +. y1)
	else
		let (a, b) = rzut (x, y) (x1, y1) (x2, y2) in
			(2. *. a -. x, 2. *. b -. y);;


let zloz ((x1, y1): point) ((x2, y2): point) (papier: kartka)=
	(
		(
			fun (x, y) -> 
				let punkt_symetryczny = symetria (x, y) (x1, y1) (x2, y2)
				and iloczyn_wektorowy = ((x2 -. x1) *. (y -. y1)) -. ((x -. x1) *. (y2 -. y1))
				in 
					if iloczyn_wektorowy > 0. then
						papier (x, y) + papier (punkt_symetryczny) 
					else if iloczyn_wektorowy =. 0. then
						papier(x, y)
					else
						0
		): kartka
	)

let kolko (x1, y1) r =
	(
		(
			fun (x, y) ->
				if (x1 -. x) *. (x1 -. x) +. (y1 -. y) *. (y1 -. y) <= r *. r then
					1
				else
					0
		): kartka
	)


(*funkcja pomocnicza do fold_lefta do funkcji skladaj*)
let pomoc a (p1, p2) =
	zloz p1 p2 a
;;

let skladaj l k =
	fold_left pomoc k l;;

(*Testy*)

(*ze smurfa z zad arytmetyka*)
let zle = ref 0
let test n b =
  if not b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
    incr zle
  end;;
(*koniec ze smurfa*)

(*moje testy*)


let a = prostokat (0., 0.) (8., 10.);;
let a = zloz (0., 0.) (8., 10.) a;; (*zlozenie na ukos*)
test 1 (a (2., 4.) = 2);;
test 2 (a (2., 0.1) = 0);;
test 3 (a (0.5, 9.5) = 1);;
let a = zloz (0., 6.) (1., 6.) a;; (*skladanie wzdluz prostej wertykalnej*)
test 4 (a (1., 6.5) = 4);;
test 5 (a (7., 7.) = 0);;
test 6 (a (0.1, 10.1) = 2);; (*"poza zainicjowana kartka" powinny pojawic sie dwa przeciecia*)

let a = zloz (6., 6.) (3., 9.) a;;
test 7 (a (4., 6.1) = 6);;

let a = kolko (4., 4.) 2.;;
test 8 (a (4., 6.) = 1);;
let a = zloz (0.,0.) (1., 1.) a;;
test 9 (a (4., 4.) = 1);;(*sprawdzenie na zgieciu*)
test 10 (a (3.99, 4.) = 2);;

let a = prostokat (0.,0.) (8., 10.);;
let b = [((0., 0.), (8., 10.)); ((0., 6.), (1., 6.)); ((6., 6.), (3., 9.))];;

let a = skladaj b a;;
test 11 (a (4., 6.1) = 6);;
test 12 (a (-.0.01, 7.) = 2);;



(*koniec moich testow*)














