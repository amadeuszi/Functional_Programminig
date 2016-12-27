open List;;

type wartosc = float * float * bool;;
(*true jezeli przedzial jest ciagly, false jak ma dziure*)

let abs x = if x > 0. then x else -.x;;

let rec fold_left f a l =
  match l with
    |[] -> a
    |h::t -> fold_left f (f a h) t;;

(*usuwa wartosc nan z listy floatów*)
let filter l = 
  fold_left (fun a h -> if ((compare h nan)=0) then a else h::a) [] l;;


let lista_mn ((x, y, _) : wartosc) ((a, b, _) : wartosc) = 
  filter [x*.a; x*.b; y*.a; y*.b];;


let wartosc_dokladnosc x p =
  (x -. p /. 100. *. abs x, x +. p /. 100. *. abs x, true);;


let wartosc_od_do x y =
  (x,y, true);;

let wartosc_dokladna x = 
  (x, x, true);;

let in_wartosc (w:wartosc) x = 
  match w with
    | (a, b, true)  -> (a <= x && x <= b)
    | (a, b, false) -> (x <= a || b <= x);;

let min_wartosc (w:wartosc) =
  match w with
    |(x, _, true) -> x
    |(x, _, false) -> if x = infinity then infinity else neg_infinity;; 


let max_wartosc (w:wartosc) =
  match w with
    |(_, y, true) -> y
    |(_, y, false) ->if y = neg_infinity then neg_infinity else infinity;;


let sr_wartosc (w:wartosc) =
  match w with
    |(x, y, true) -> ((x +. y) /. 2.0)
    |(x, y, false) -> nan;;

let rec plus (x : wartosc) (y : wartosc) =
  match x with
    | (a, b, true) -> ( match y with
                        | (c, d, true) -> ((a +. c, b +. d, true) : wartosc)
                        | (c, d, false) ->((b +. c, a +. d, false) : wartosc)
                      )
    | (a, b, false) -> ( match y with
                         |(c, d, true) -> plus y x
                         |(c, d, false) ->(neg_infinity, infinity, true):wartosc
                       );;

let minus ((a, b, x):wartosc) ((c, d, y):wartosc) =
  plus (a, b, x) ((-.d), (-.c), y);;

let f_min a h =
  if h < a then h else a;;

let f_max a h =
  if h < a then a else h;;

let szukaj_min (x : wartosc) (y : wartosc) = 
  fold_left f_min (hd (lista_mn x y)) (lista_mn x y);;

let szukaj_max (x : wartosc) (y : wartosc) =
  fold_left f_max (hd (lista_mn x y)) (lista_mn x y);;

let szukaj_min_max (x : wartosc) (y : wartosc) =
  (szukaj_min x y, szukaj_max x y, true);;



let uporzadkuj ((a, b, x): wartosc) ((c, d, y): wartosc) = (*tylko zbiory z trzecia wartoscia true*)
  if a < c then
    true
  else
    (if (a = c) then
       if b <= d then
         true 
       else
         false
     else (* c < a *)
       false
    );;




let rec suma_pomoc ((a, b, x): wartosc) ((c, d, y): wartosc) =
  if (x = true && y = true) then
    if ((a = c && a = neg_infinity) || (b = d && b = infinity)) then 
      (min a c, max b d, true)
    else if (c <= b) then
      (neg_infinity, infinity, true)
    else
      (b, c, false)
  else if (x = true && y = false) then
    if a = neg_infinity then
      suma_pomoc (neg_infinity, max b d, true) (d, infinity, true)
    else (*b=infinity*)
      suma_pomoc (neg_infinity, c, true) (min a d, infinity, true)
  else if (x = false && y = true) then
    suma_pomoc (c, d, y) (a, b, x)
  else  (*x = false && y = false*)
    (if max a c < min b d then
       (max a c, min b d, false)
     else
       (neg_infinity, infinity, true)
    )
;;

let suma ((x): wartosc) ((y): wartosc) =
  match x with
    |(a, b, true) -> (match y with
                       | (c, d, true) ->
                           if uporzadkuj x y then suma_pomoc x y else suma_pomoc y x
                       | _ -> suma_pomoc x y
                     )
    | _ -> suma_pomoc x y;;



let rec razy ((x):wartosc) ((y):wartosc) = 
  match x with
    |(a, b, true) -> (match y with 
                       |(c, d, true) -> szukaj_min_max x y
                       |(c, d, false) -> suma (razy ((a, b, true) : wartosc) ((neg_infinity, c, true):wartosc)) 
                                           (razy ((a, b, true) : wartosc) ((d, infinity, true):wartosc))
                     )
    |(a, b, false) -> (match y with
                        |(c, d, true) -> razy y x
                        |(c, d, false) -> suma (razy ((neg_infinity, a, true): wartosc) (y :wartosc)) 
                                            (razy ((b, infinity, true) : wartosc) (y : wartosc))
                      )
;;

let odwroc (x : wartosc) = 
  match x with
    |(a, b, true) -> 
        if in_wartosc x 0. then 
          if (a < 0. && 0. < b) then
            ( 1. /. a, 1. /. b, false)
          else if (a = 0. && 0. < b) then
            (1. /. b, infinity, true)
          else 
            (neg_infinity, 1. /. a, true)
        else
          (min (1. /. a) (1. /. b), max (1. /. a) (1. /. b), true)
    |(a, b, false) -> 
        if in_wartosc x 0. then
          if (a > 0. && b > a) then
            (neg_infinity, infinity, true)
          else if (b < 0. && a < b) then
            (neg_infinity, infinity, true)
          else if a = 0. && b <> 0. then
            (neg_infinity, 1. /. b, true)
          else if a <> 0. && b = 0. then
            ((1. /. a), infinity, true)
          else
            (neg_infinity, infinity, true)
        else
          ((1. /. a), (1. /. b), true);;


let podzielic x y = razy x (odwroc y);;







