
(* ========== Vaja 1: Uvod v OCaml  ========== *)

(*----------------------------------------------------------------------------*]
 Funkcija [square] vrne kvadrat podanega celega števila.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # square 2;;
 - : int = 4
[*----------------------------------------------------------------------------*)
let rec square x = x * x (* rec = rekurzivno ponavljamo, sqaure = ime funkcije, x argument, funkcija vrne x * x*)

(* sqaure 2;; *)
(* square 2.3;; Vrne napako. Uporabiti bi morali *.*)

let square x = x * x (*To funkcijo lahko napišemo tudi brez rekurzije*)

let square = (fun x -> x * x)

(*----------------------------------------------------------------------------*]
 Funkcija [middle_of_triple] vrne srednji element trojice.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # middle_of_triple (true, false, true);;
 - : bool = false
[*----------------------------------------------------------------------------*)
(* Python: triple[1] *)

let rec middle_of_triple triple =
  match triple with
  |(x, y, z) -> y

let middle_of_triple = function
  |(x, y, z) -> y

let middle_of_triple (x, y, z) = y

let middle_of_triple (_, y, _) = y (* x in z v tem primeru nista niti pomembna in jima ne določimo vrednosti*)

(*middle_of_triple;; Funkcija, ki sprejme produkt treh stvari in vrne neki tipa b, kar je srednji elemet*)

(*----------------------------------------------------------------------------*]
 Funkcija [starting_element] vrne prvi element danega seznama. V primeru
 prekratkega seznama vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # starting_element [1; 2; 3; 4];;
 - : int = 1
[*----------------------------------------------------------------------------*)

 let rec starting_element list =
  match list with 
  | [] ->  failwith "Empty list has no starting element"  (*V primeru praznega seznama vrne napako*)
  |x :: xs ->   x  (* Tukaj imamo sestavljen seznam. In mi hočemo prvi element.*) 

(* starting_element;; Prejme alfa list. Torej seznam elementov tipa alfa in vrne element tipa alfa*)

let rec starting_element list = function
  | [] -> failwith "Empty list has no starting element"
  | x :: xs -> x

(* Tretje opcije, ker matchamo že direktno v funkciji ne moremo uporabit, ker imamo dve opciji :
let rec starting_element (x :: xs) = x. V primeru praznega seznama napaka. Ocaml nas na to tudi opozori*)

(*let _ = assert (starting_element [1; 2; 3; 4] = 2)*)

(*----------------------------------------------------------------------------*]
 Funkcija [multiply] zmnoži vse elemente seznama. V primeru praznega seznama
 vrne vrednost, ki je smiselna za rekurzijo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # multiply [2; 4; 6];;
 - : int = 48
[*----------------------------------------------------------------------------*)

let rec multiply list =
  match list with
  | [] -> 1
  | x :: xs ->  x * multiply xs

let rec multiply = function
  | [] -> 1
  | x :: xs -> x * multiply xs (* Zmnožimo elemente preostanka seznama in to pomnožimo z x*)

(*----------------------------------------------------------------------------*]
 Napišite funkcijo ekvivalentno python kodi:

  def sum_int_pairs(pair_list):
      if len(pair_list) == 0:
        return []
      else:
        x, y = pair_list[0]
        return [x + y] + sum_int_pairs(pair_list[1:])

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # sum_int_pairs [(1, -2); (3, 4); (0, -0)];;
 - : int list = [-1; 7; 0]
[*----------------------------------------------------------------------------*)

let rec sum_int_pairs pair_list =
  match pair_list with
  | [] -> []
  | el :: els -> (
    match el with
    | (x, y) -> x + y :: sum_int_pairs els
  )
    
let rec sum_int_pairs  = function
  | [] -> []
  | (x, y) :: els -> (x + y) :: sum_int_pairs els (* ali je prazen seznam in vrne prazen seznam ali pa je 
                                                     seznam sestavljen in je prvi element (x, y)*)


(*----------------------------------------------------------------------------*]
 Funkcija [get k list] poišče [k]-ti element v seznamu [list]. Številčenje
 elementov seznama (kot ponavadi) pričnemo z 0. Če je k negativen, funkcija
 vrne ničti element. V primeru prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let rec get k list =
  if k <= 0 then
    match list with
    | [] -> failwith "List too short"
    | x :: xs -> x
  else
    match list with
    | [] -> failwith "List too short"
    | x :: xs -> get (k -1) xs 

(* Zgornja funkcija deluje, a z njo nismo preveč zadovoljni. Funkcija mora vrniti element in najprej preverimo ali
ga lahko.*)
let rec get k list =
  match list with
  | [] -> failwith "List too short"
  | x :: xs -> if k <= 0 then x else get (k - 1) xs

      

(*----------------------------------------------------------------------------*]
 Funkcija [double] podvoji pojavitve elementov v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)

let rec double list = 
  match list with
  | [] -> []
  | x :: xs -> x :: x :: double xs

let rec double = function
  | [] -> []
  | x :: xs -> x :: x :: double xs 

(*----------------------------------------------------------------------------*]
 Funkcija [insert x k list] na [k]-to mesto seznama [list] vrine element [x].
 Če je [k] izven mej seznama, ga funkcija doda na začetek oziroma na konec.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec insert x k list =
  match list with
  | [] -> [x]
  | y :: ys -> 
      if k < 0 then
        x :: y :: ys
      else
        y :: (insert x (k-1) ys)

(* Malo ekstra *)
let rec insert x k = function
  | [] -> [x]
  | y :: ys when k <= 0 -> x :: y :: ys
  | y :: ys (* when k > 0*) -> y :: ( insert x (k-1) ys)

let rec x_is_in_list x = function
  | [] -> false
  | y :: ys when x = y -> true
  | y :: ys -> x_is_in_list x ys



(*----------------------------------------------------------------------------*]
 Funkcija [divide k list] seznam razdeli na dva seznama. Prvi vsebuje prvih [k]
 elementov, drugi pa vse ostale. Funkcija vrne par teh seznamov. V primeru, ko
 je [k] izven mej seznama, je primeren od seznamov prazen.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)

let rec divide k list =
  match (k, list) with
  | (_, []) -> ([], [])
  | (k, list) when k <= 0 -> ([], list)
  | (k, x :: xs) ->
      let (list1, list2) = divide (k - 1) xs in
	    (x :: list1, list2)


(*----------------------------------------------------------------------------*]
 Funkcija [rotate n list] seznam zavrti za [n] mest v levo. Predpostavimo, da
 je [n] v mejah seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let one_rotate list =
  match list with
  | [] -> []
  | x :: xs -> xs @ x :: []

let rec rotate n list =
  match n with
  | 0 -> list
  | _ -> rotate (n - 1) (one_rotate list)

(*----------------------------------------------------------------------------*]
 Funkcija [remove x list] iz seznama izbriše vse pojavitve elementa [x].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)

let rec remove x list =
  match list with
  | [] -> []
  | y :: ys -> 
    if x == y then
       remove x ys
    else
      y :: remove x ys
      


(*----------------------------------------------------------------------------*]
 Funkcija [is_palindrome] za dani seznam ugotovi ali predstavlja palindrom.
 Namig: Pomagaj si s pomožno funkcijo, ki obrne vrstni red elementov seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)
let rec rotate list =
  match list with
  | [] -> []
  | x :: xs ->  (rotate xs) @ x :: []


let rec is_palindrome list =
  match list with
  | [] -> true
  | x :: xs -> rotate list = list

  let is_palindrome list =
  let rec reverse = function
    | x :: xs -> reverse xs @ [x]
	  | [] -> []
  in
  list = reverse list

(*----------------------------------------------------------------------------*]
 Funkcija [max_on_components] sprejme dva seznama in vrne nov seznam, katerega
 elementi so večji od istoležnih elementov na danih seznamih. Skupni seznam ima
 dolžino krajšega od danih seznamov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)
let rec max_on_components list1 list2 =
  match (list1, list2) with
  | (x :: xs, y :: ys) -> max x y :: max_on_components xs ys
  | _ -> []

(*----------------------------------------------------------------------------*]
 Funkcija [second_largest] vrne drugo največjo vrednost v seznamu. Pri tem se
 ponovitve elementa štejejo kot ena vrednost. Predpostavimo, da ima seznam vsaj
 dve različni vrednosti.
 Namig: Pomagaj si s pomožno funkcijo, ki poišče največjo vrednost v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)

let second_largest list =
  let rec largest = function
    | [] -> failwith "List is too short."
	  | x :: [] -> x
	  | x :: xs -> max x (largest xs)
  in
  largest (delete (largest list) list)