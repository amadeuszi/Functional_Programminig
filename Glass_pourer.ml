(*Autor: Amadeusz Iwanowski*)
(*Code review: Marcin Fryz*)


(*tworzy liste n zer*)
let stworz_liste_zer n =
	let rec pom acu k =
		if k = n then 
			acu
		else
			pom (0 :: acu) (k + 1) 
	in pom [] 0;;

(*dodaje kazdy element listy do kolejki q*)
let rec dodaj_liste_do_kolejki q l wartosc =
	match l with
		|h :: t -> Queue.add (h, wartosc) q; dodaj_liste_do_kolejki q t wartosc;
		|_ -> ();;



let przelewank l = (*robie to na listach, poniewaz jest mi ³atwiej to wszzystko przetwarzac, to jest funkcja pomocnicza, ktora opreuje na listach*)
	let licznik = ref (-1) (*licznik, ktory bedzie trzymal dlugosc drogi*)
	and n = List.length l
	and cel = List.rev (List.fold_left (fun a (x, y) -> y :: a) [] l) (*Lista do ktorej chcemy dojsc*)
	and pojemnosci = List.rev (List.fold_left (fun a (x, y) -> x :: a) [] l) (*Lista pojemnosci*)
	and nwd a b = (*zwraca najwiekszy wspolny dzielnik a i b*)
		if a = 0 && b = 0 then 
			0
		else
			let c = ref 0 and a1 = ref a and b1 = ref b in
				while !b1 <> 0 do
				begin
					c := !a1 mod !b1;
					a1 := !b1;
					b1 := !c;
				end
				done; !a1 in
			let neighbours vertice = (*funkcja dostaje liste i wypluwa wszystkie nastepne mozliwosci ruchow*)
				let dolej vertice = 
				let rec pom acu przed lista pojemnosci =
					match lista, pojemnosci with
						|h1 :: t1, h2 :: t2 -> 
							if h1 < h2 then 
								pom ((przed @ (h2 :: t1)) :: acu) (przed @ [h1]) t1 t2
							else 
								pom acu (przed @ [h1]) t1 t2
						|_-> acu
				in pom [] [] vertice pojemnosci
				and wylej vertice =
					let rec pom acu przed lista =
						match lista with
							|h :: t -> 
								if h > 0 then
									pom ((przed @ (0 :: t)) :: acu) (przed @ [h]) t
								else 
									pom acu (przed @ [h]) t
							|[] -> acu
					in pom [] [] vertice
				and przelej vertice =
					let tab = Array.of_list pojemnosci
					and r = Array.of_list vertice
					and acu = ref [] in
						for i = 0 to n - 1 do
							let x = r.(i) in
								if x > 0 then
									for j = 0 to n - 1 do
									begin
									if j <> i then
										let y = r.(j) in
										begin
											if tab.(j) < r.(j) + r.(i) then
											begin
												r.(j) <- tab.(j);
												r.(i) <- x - (tab.(j) - y);
											end
									else
									begin
										r.(j) <- r.(j) + r.(i);
										r.(i) <- 0;
									end;
										acu := (Array.to_list r) :: !acu;
										r.(i) <- x;
										r.(j) <-y;
										end
									end
									done
						done; !acu
				in (dolej vertice, wylej vertice, przelej vertice)
			and d = List.fold_left (fun a h -> nwd a h) 0 pojemnosci (* NWD (x1,x2,x3,...,xn) *)
			in
			let cel_mozliwy_do_osiagniecia = List.fold_left (fun a h -> a && (if d = 0 then true else (h mod d = 0))) true cel
			and jedno_puste = List.fold_left (fun a h -> h = 0 || a) false cel 
			and jedno_pelne = List.fold_left2 (fun a h1 h2 -> h1 = h2 || a) false cel pojemnosci in
				if l = [] then
					0
				else if cel_mozliwy_do_osiagniecia && (jedno_puste || jedno_pelne) then     (*program wlasciwy*)
					begin
						let queue = Queue.create () and table = Hashtbl.create 100000 in
						Queue.add (stworz_liste_zer n, 0) queue; (*algorytm to BFS*)
						while not (Queue.is_empty queue) do
							let (vertice, wartosc) = Queue.take queue in
							if not (Hashtbl.mem table vertice) then (*gdy nie sprawdzilismy jeszcze tej konfiguracji to sprawdzamy*)
							begin
								Hashtbl.add table vertice true;
								let a, b, c = neighbours vertice in
								begin
									dodaj_liste_do_kolejki queue a (wartosc + 1);
									dodaj_liste_do_kolejki queue b (wartosc + 1);
									dodaj_liste_do_kolejki queue c (wartosc + 1);
								end;
								if vertice = cel then
								begin
									licznik := wartosc;
									Queue.clear queue;
								end
							end
						done; !licznik
					end
				else -1;;


let przelewanka l = przelewank (Array.to_list l);; 



assert (przelewanka [| (5, 3); (7, 2); (11, 10) |] = -1);;
assert (przelewanka [| (5, 3); (5, 3); (11, 10); (2, 0) |] = 7);;
assert (przelewanka [| (2, 1); (2, 0) |] = -1);;
assert (przelewanka [| (2, 1); (7, 2); (22, 11); (100, 100) |] = 12);;