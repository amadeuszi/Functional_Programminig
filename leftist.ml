(**********************************************************)
(*         Zadanie:Drzewa Lewicowe                        *)
(*         Autor: Amadeusz Iwanowski                      *)
(*         Code Review: Michał Kuźba                      *)
(**********************************************************)


(*Pierwsza wartosc to "waga" danego elementu, druga wartosc to lewicowosc drzewa, czyli dlugosc prawej sciezki do liscia*)
type 'a tree = Node of 'a * int * 'a tree * 'a tree | Leaf;;

type 'a queue = 'a tree;;

(*sprawdza, czy drzewo x ma w korzeniu "wage" mniajsza od "wagi" y, funkcja pomocnicza przy sklejaniu drzewa*)
let check x y = 
  match (x, y) with
    |(Node(a, _, _, _), Node(b, _, _, _)) -> a <= b
    |(Node(_, _, _, _), Leaf )-> true
    |(Leaf, Leaf) -> true
    |(Leaf, Node(_, _, _, _))-> true
;;

(*funkcja pomocnicza, podaje lewicowosc danego drzewa*)
let rank t = 
  match t with
    |Node(_, a, _, _)->a
    |Leaf -> (-1);;

(*skleja dwa drzewa, po lewej drzewo musi miec mniejsza lub rowna wage*)
let rec merge x y =
  if check x y then
    match (x, y) with
      |(Node(importance, leftist, left, right), Node(importance2, leftist2, left2, right2)) ->
          let merged = merge right y in
          let le = rank left and ri = rank (merged) (*lewicowosc lewego poddrzewa i prawego poddrzewa w czasie sklejania*)
          in
            if le >= ri then
              (Node (importance, ri + 1, left, merged))
            else
              (Node (importance, le + 1, merged, left))
      |(Node(_, _, _, _), Leaf) -> x
      |(Leaf, Node(_, _, _, _)) -> y
      |(Leaf, Leaf) -> Leaf
  else
    merge y x;;

let empty = Leaf;;

let add e q = merge (Node(e, 0, Leaf, Leaf)) q;;

exception Empty;;

let join x y = merge x y;;



let delete_min q = 
  match q with
    |Node(importance, leftist, left, right) -> (importance, join left right)
    |Leaf->raise Empty;;

let is_empty q =
  match q with
    |Node(_, _, _, _) -> false
    |Leaf -> true;;


