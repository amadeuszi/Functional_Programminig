(*
* PSet - Polymorphic sets
* Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version,
* with the special exception on linking described in file LICENSE.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(***********************************************************)
(*                                                         *)
(*              Autor: Amadeusz Iwanowski                  *)
(*              Code review: Jakub Grzywacz                *)
(*                                                         *)
(***********************************************************)


(*lewy wiekszy -> 2
  lewy mniejszy -> -2
  rowne -> 0
  w innym wypadku -> 1
*)
let compare2 x y =
  match x, y with
    |(a, b), (c, d) -> 
        if a > d + 1 then
          2
        else if b + 1 < c then
          -2
        else if (a = c && b = d) then 
          0
        else(*nieporownywalne, przecinaja sie jakos, albo jeden zawiera drugi, albo sa bardzo bliskie tj. odleglosc pomiedzy nimi to 1*)
          1;;

(*sprawdza czy liczba x jest w zbiorze (a, b)*)
let in_set x (a, b) =
  if a = x && a = b then 
    0 
  else if a < x && x < b && a <> b then
    1
  else if a = x && a <> b then
    7
  else if b = x && a <> b then
    8 
  else if x < a then
    -2
  else (* x > b *)
    2

let obciecie_l x (a, b) =
  (a, x - 1)

let obciecie_r x (a, b) =
  (x + 1, b)

let min_max (a, b) (c, d) =
  (min a c, max b d) 


type set =
    | Empty
    | Node of set * (int * int) *  set * int

type t =
    {
      cmp : int * int -> int * int -> int;
      set : set;
    }

let height = function
  | Node (_, _, _, h) -> h
  | Empty -> 0

let make l k r = Node (l, k, r, max (height l) (height r) + 1)

let bal l k r =
  let hl = height l in
  let hr = height r in
    if hl > hr + 2 then
      match l with
        | Node (ll, lk, lr, _) ->
            if height ll >= height lr then make ll lk (make lr k r)
            else
              (match lr with
                | Node (lrl, lrk, lrr, _) ->
                    make (make ll lk lrl) lrk (make lrr k r)
                | Empty -> assert false)
        | Empty -> assert false
    else if hr > hl + 2 then
      match r with
        | Node (rl, rk, rr, _) ->
            if height rr >= height rl then make (make l k rl) rk rr
            else
              (match rl with
                | Node (rll, rlk, rlr, _) ->
                    make (make l k rll) rlk (make rlr rk rr)
                | Empty -> assert false)
        | Empty -> assert false
    else Node (l, k, r, max hl hr + 1)

let rec min_elt = function
  | Node (Empty, k, _, _) -> k
  | Node (l, _, _, _) -> min_elt l
  | Empty -> raise Not_found

let rec max_elt = function
  | Node (_, k, Empty, _) -> k
  | Node (_, _, r, _) -> max_elt r
  | Empty -> failwith "Nope"

let rec remove_max_elt = function
  | Node (l, _, Empty, _) -> l
  | Node (l, k, r, _) -> bal l k (remove_max_elt r)
  | Empty -> invalid_arg "ISet.remove_max_elt"


let rec remove_min_elt = function
  | Node (Empty, _, r, _) -> r
  | Node (l, k, r, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "ISet.remove_min_elt"

let merge t1 t2 =
  match t1, t2 with
    | Empty, _ -> t2
    | _, Empty -> t1
    | _ ->
        let k = min_elt t2 in
          bal t1 k (remove_min_elt t2)


let empty = { cmp = compare2; set = Empty }

let is_empty x = 
  x.set = Empty

let rec add_one cmp x = function
  | Node (l, k, r, h) ->
      let c = compare2 x k in
        if c = 0 then Node (l, k, r, h)
        else if c < 0 then
          let nl = add_one cmp x l in
            bal nl k r
        else
          let nr = add_one cmp x r in
            bal l k nr
  | Empty -> Node (Empty, x, Empty, 1)



let rec join cmp l v r =
  match (l, r) with
      (Empty, _) -> add_one cmp v r
    | (_, Empty) -> add_one cmp v l
    | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
        if lh > rh + 2 then bal ll lv (join cmp lr v r) else
        if rh > lh + 2 then bal (join cmp l v rl) rv rr else
          make l v r;;

(*robi to samo co funkcja join tylko nie trzeba jej podawac argumentu v, 
  sama go znajduje, wynik to drzewo AVL, a trzeba jej podac dwa drzewa AVL, 
  gdzie lewe ma "mniejsze" przedzialy*)
let join2 l r =
  match l, r with
    |Empty, Empty -> Empty
    |Empty, _ -> r
    |_, Empty -> l
    |_ -> 
        let k = min_elt r in
          join compare2 l k (remove_min_elt r);;

(*tutaj dodalem tylko dodatkowe przypadki, ale te najwazniejsze sa zrobione tak samo jak w pSet, 
  wszystkie inne funkcje beda sie opieraly na tej funkcji np Add, itd.,
  beda od niej zalezne*)
let split x { cmp = cmp; set = set } =
  let rec loop x = function
      Empty ->
        (Empty, false, Empty)
    | Node (l, v, r, _) ->
        let c = in_set x v in (*te przypadki wynikaja z def in_set*)
          if c = 0 then (l, true, r)
          else if c = -2 then
            let (ll, pres, rl) = loop x l in (ll, pres, join cmp rl v r)
          else if c = 2 then
            let (lr, pres, rr) = loop x r in (join cmp l v lr, pres, rr)
          else if c = 1 then
            (add_one cmp (obciecie_l x v) l, true, add_one cmp (obciecie_r x v) r)
          else if c = 7 then
            (l, true, add_one cmp (obciecie_r x v) r)
          else
            (add_one cmp (obciecie_l x v) l, true, r)
  in
  let setl, pres, setr = loop x set in
    { cmp = cmp; set = setl }, pres, { cmp = cmp; set = setr };;

(*zwraca drzewo ktore ma wartosci ostro wieksze od x*)
let left_split x set =
  let (a, _, _) = split x set in
    a.set;;

(*zwraca drzewo ktore ma wartosci ostro mniejsze od x*)
let right_split x set =
  let (_, _, c) = split x set in
    c.set;;

let remove_interval (a, b) obj_set =
  join2 (left_split a obj_set) (right_split b obj_set);;

let remove x set_a = 
  { cmp = compare2; set = remove_interval x set_a };;


(* add_tree dodaje przedzial do drzewa,
   1.) rozdziela drzewo na dwie czesci:
   ltree - wszystkie wartosci ostro mniejsze od a, 
   rtree - wszystkie wartosci wieksze od b (log n)
   2.) sprawdza czy w lewym lub w prawym drzewie sa jakies przedzialy 
   ktore sie merguja z dodawanym przedzialem:
   dokladnie sprawdza tylko dwie wartosci : najwieksza w lewym i najmniejsza w prawym: czas: O(log n)
   3.)jezeli zachodzi potrzeba to usuwa przedzialy z pktu 2.) i zapamietuje juz zmergowany przedzial. (log n)
   4.)merguje dwa drzewa lewe i prawe (log n)
   5.)dodaje zmergowany interwal (log n)
*)
let add_tree (a, b) set_a =
  let ltree = left_split a set_a and rtree = right_split b set_a in
    match ltree, rtree with
      |Empty, Empty ->
          add_one compare2 (a, b) Empty
      |Empty, _ ->
          let mini = min_elt rtree in
            if compare2 (mini) (a, b) = 1 then
              add_one compare2 (min_max mini (a, b)) (remove_min_elt rtree)
            else
              add_one compare2 (a, b) rtree
      |_, Empty ->
          let maxi = max_elt ltree in
            if compare2 maxi (a, b) = 1 then
              add_one compare2 (min_max maxi (a, b)) (remove_max_elt ltree)
            else
              add_one compare2 (a, b) ltree
      |_ ->    (* ltree <> Empty && rtree <> Empty *)
          let mini = min_elt rtree and maxi = max_elt ltree in
            match compare2 mini (a, b), compare2 maxi (a, b) with
              |1, 1 ->
                  add_one compare2 (min_max mini (min_max maxi (a, b))) (join2 (remove_max_elt ltree) (remove_min_elt rtree))
              |1, _ ->
                  add_one compare2 (min_max mini (a, b)) (join2 ltree (remove_min_elt rtree))
              |_, 1 ->
                  add_one compare2 (min_max maxi (a, b)) (join2 (remove_max_elt ltree) rtree)
              |_ ->
                  add_one compare2 (a, b) (merge ltree rtree);;

let add x i_set =
  {cmp = compare2; set = add_tree x i_set};;

let in_set_second x (a, b) =
  if a <= x && x <= b then
    0
  else if x < a then
    -1
  else
    1;;


let mem x { cmp = cmp; set = set } =
  let rec loop = function
    | Node (l, k, r, _) ->
        let c = in_set_second x k in
          c = 0 || loop (if c < 0 then l else r)
    | Empty -> false in
    loop set

let exists = mem

let iter f { set = set } =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _) -> loop l; f k; loop r in
    loop set

let fold f { cmp = cmp; set = set } acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _) ->
        loop (f k (loop acc l)) r in
    loop acc set

let elements { set = set } = 
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _) -> loop (k :: loop acc r) l in
    loop [] set


let (+..) a b =
  if a = max_int || b = max_int then
    max_int
  else if a + b < 0 then
    max_int
  else
    a + b

let dlugosc (a, b) =
  if b - a < 0 then
    max_int
  else
    (b - a) +.. 1;;


let rec zlicz tr =
  match tr with
    |Node (l, k, r, _) -> dlugosc k +.. zlicz l +.. zlicz r
    |Empty -> 0;;

let below x { set = set } =
  let (tree, exists, _) = split x { cmp = compare2; set = set } in
    zlicz tree.set +.. if exists then 1 else 0 ;;

(*testowanie rozlacznych przedzialow*)

let a = empty;;

assert (is_empty a);;

let a = remove (501, 1748) a;; (*nie ma co usuwac, wiec nie powinno sie nic stac*)

assert (is_empty a);;

let a = add (352, 898) a;;

assert (not (is_empty a));;

let a = remove (3760, 6333) a;;(*nic sie nie dzieje*)

let a = add (222, 225) a;;

let a = add (1000, 1200) a;;

let a = add (227,227) a;;

let a = add (202, 212) a;;

let a = add (900, 912) a;;

assert (below 900 a = 564);;

assert (elements a = [(202, 212); (222, 225); (227, 227); (352, 898); (900, 912); (1000, 1200)]);;

(*testowanie mergowania sie przedzialow*)

let a = add (0, 10000) a;; (*wszystko merguje sie w jeden przedzial, ltree = Empty rtree = Empty*)

assert (elements a = [(0, 10000)]);;

let a = remove (10, 10000) a;; (*obciecie z lewej strony*)

let a = add (11, 15) a;; 

let a = add (10, 10) a;; (*lewe i prawe drzewo niepuste, mergowanie z obu stron*)

assert (elements a = [(0, 15)]);;

let a = add (17, 18) a;;

let a = add (19, 20) a;; (*rtree puste, mergowanie z prawej*)

assert (elements a = [(0, 15); (17, 20)]);;

let a = add (200, 9632) a;;

let a = add (-1, -1) a;; (*mergowanie z lewej, ltree puste*)

assert (elements a = [(-1, 15); (17, 20); (200, 9632)]);; 

(*testowanie remove i mem*)

let a = remove (8425, 9178) a;; (*usuwam ze srodka jednego z przedzialow*)

assert (mem 8424 a);;

assert (not (mem 8425 a));;

assert (mem 0 a);;

assert (mem 9179 a);;

assert (not (mem 9178 a));;

let a = remove (8001, 9631) a;; (*usuwam czesci dwoch przedzialow*)

assert (mem 8000 a);; (*sprawdzam czy sie usunelo to co powinno*)

assert (not (mem 8001 a));;

assert (not (mem 9631 a));;

assert (mem 9632 a);;

assert (mem 222 a);; (*randomowy test*)

(*iter, fold*)

let a = add (4, 5) a;;

let a = add (3,3) a;;

let a = add (4, 5) a;;

let a = add (3,3) a;;

let a = add (4, 5) a;;

let a = remove (221, 10000) a;;

let a = add (25, 35) a;;

let a = add (38, 40) a;;

assert ((fold (fun (a, b) x -> x + a + b) a 0) = 609) ;;

(*split*)
let (x, y, z) = split 39 a;;

assert y;;

assert (elements x = [(-1, 15); (17, 20); (25, 35); (38, 38)]);;

assert (elements z = [(40, 40); (200, 220)]);;

(*below*)

assert (below 222 a = 56)

let a = add (min_int, max_int) a;;

assert (below max_int a = max_int);;

assert (below 10 a = max_int);;

let a = remove (min_int / 3, max_int / 3) a;;

assert (below max_int a = max_int);;



