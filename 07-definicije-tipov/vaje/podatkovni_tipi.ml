(* ========== Vaja 3: Definicije Tipov  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute.
 Oglejmo si dva pristopa k izboljšavi varnosti pri uporabi valut.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

let my_euros = 34.45

let euro_to_dollar euros = 1.2 *. euros

(* Kaj je zdaj problem? V terminal: my_euros, euro_to_dollar,
let my_dollars = euro_to_dollar my_euros, let my_what= euro_to_dollar my dollars:
tudi to deluje, čeprav to ni pravilno*)

(* Prva naravna stvar, ki jo lahko v Ocamlu naredimo je, da tipe poimenujemo.*)

type euro = float
type dollar = float

let my_euros : euro = 34.56 
let my_euros = (34.56 : euro)

let euro_to_dollar (euro : euro) : dollar = 1.2 *. euro
(* euri; ki jih dobimo bodo euri in vrne dolarje*)

(* my_euros je tipa euro, euro_to_dolar je funkcija iz eura v doalr.
let my_dollars = euro_to_dollar my_euros. Problem:
let my_dollars = euro_to_dollar my_dollar: to še vedno legalno, ker je dollar in euro float*)

(* Imamo kodo, ki je bolj informativna, ampak še vedno ni v redu.*)

(* Definiramo dva tipa euro in dollar in vsak bo imel svoj konstruktor.*)


(*----------------------------------------------------------------------------*]
 Definirajte tipa [euro] in [dollar], kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število.
 Nato napišite funkciji [euro_to_dollar] in [dollar_to_euro], ki primerno
 pretvarjata valuti (točne vrednosti pridobite na internetu ali pa si jih
 izmislite).

 Namig: Občudujte informativnost tipov funkcij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dollar_to_euro;;
 - : dollar -> euro = <fun>
 # dollar_to_euro (Dollar 0.5);;
 - : euro = Euro 0.4305
[*----------------------------------------------------------------------------*)

type euro = Euro of float
type dollar = Dollar of float

let dollar_to_euro dollar=
  match dollar with
  | Dollar d -> Euro( 0.7 *. d)

let euro_to_dolar (Euro e) = Dollar (1.2 *. e)

(* dollar_to_euro, let my_euro = Euro 43,  let my_dollars = euro_to_dollar my_euro
Dobimo poleg tudi konstruktor. Zdaj ne moramo več narediti napake:
let my_dollars = euro_to_dollar my_dollars*)


(*----------------------------------------------------------------------------*]
 Definirajte tip [currency] kot en vsotni tip z konstruktorji za jen, funt
 in švedsko krono. Nato napišite funkcijo [to_pound], ki primerno pretvori
 valuto tipa [currency] v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
        Ocaml sam opozori, da je potrebno popraviti funkcijo [to_pound].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # to_pound (Yen 100.);;
 - : currency = Pound 0.007
[*----------------------------------------------------------------------------*)
type currency = Yen of float | Pound of float | Krona of float | Frank of float
(* Imamo valuto in trie različne konstruktorje*)

let my_yen = Yen 1987.12
(* my_yen: Tip je sicer manj informativen, ampak saj imamo konstruktorje,
ki nam povedo kaj je*)

let my_pound = Pound 123.3

(*let to_pound currency =
  match currency with
  | Yen y -> Pound(0.05 *. y)
  | Pound p -> Pound p
  | Krona k -> Pound(0.7 *. k)*)

let to_pound currency = function
  | Yen y -> Pound(0.05 *. y)
  | Pound p -> Pound p
  | Krona k -> Pound(0.7 *. k)
  | Frank f -> Pound( 0.2 *. f)
  
(* Dodamo še franke in ta funkcija to_pound ugotovi, da smo 
pozabili na primer, ko imamo franke*)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip [list] predstavimo s konstruktorjem za prazen seznam
 [Nil] (oz. [] v Ocamlu) in pa konstruktorjem za člen [Cons(x, xs)] (oz.
 x :: xs v Ocamlu).
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*1.možnost*)
(*type int_or_bool = Int of int | Bool of Bool
type intbool_list = int_or_bool list*)
(*V praksi to bolj koristen pristop, ampak bomo raje konstruirali nov tip seznamov*)

(*----------------------------------------------------------------------------*]
 Definirajte tip [intbool_list] z konstruktorji za:
  1.) prazen seznam,
  2.) člen z celoštevilsko vrednostjo,
  3.) člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal "[5; true; false; 7]".
[*----------------------------------------------------------------------------*)

(* Nil ~ [], Cons (x, xs) ~ x :: xs*)
type intbool_list = Nil | IntCons of int * intbool_list | BoolCons of bool * intbool_list
(* Torej intbool_list je lahko prazen, lahko ima na začetku int in se
nadaljuje z intbool_list ali pa se začne z bool in se nadaljuje z intbool_list*)


let primer = IntCons(5, BoolCons(true, BoolCons(false, IntCons(7,Nil))))
(*----------------------------------------------------------------------------*]
 Funkcija [intbool_map f_int f_bool ib_list] preslika vrednosti [ib_list] v nov
 [intbool_list] seznam, kjer na elementih uporabi primerno od funkcij [f_int]
 oz. [f_bool].
[*----------------------------------------------------------------------------*)

(* Spomnimo se funkcije [map f list].
Ta sprejme seznam in fukncijo ter vrne seznam preslikanih vrednosti.
let rec map f  = function
  | [] -> []
  | x : xs -> f x :: map f xs
*) 

let rec intbool_map f_int f_bool = function
  | Nil -> Nil
  | IntCons (x, ib_list) -> IntCons(f_int x, intbool_map f_int f_bool ib_list)
  | BoolCons (x, ib_list) -> BoolCons(f_bool x, intbool_map f_int f_bool ib_list)

(* intbool_map ((+) 2) (not) primer *)


(*----------------------------------------------------------------------------*]
 Funkcija [intbool_reverse] obrne vrstni red elementov [intbool_list] seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

(* Spomnimo se funkcije za obračanje seznamov:
let reverse list =
  let rec reverse_aux acc list =
    match list with
    | [] -> acc
    | x :: xs -> reverse_aux (x :: acc) xs
  in
  reverse_aux [] list *)

let intbool_reverse ib_list =
  let rec aux_reverse acc = function
    | Nil -> Nil
    | IntCons (x, ib_list) -> aux_reverse (IntCons(x ,acc)) ib_list
    | BoolCons (x, ib_list) -> aux_reverse (BoolCons(x, acc)) ib_list
  in
  aux_reverse Nil ib_list


(*----------------------------------------------------------------------------*]
 Funkcija [intbool_separate ib_list] loči vrednosti [ib_list] v par [list]
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)

(* 5 :: true :: false :: 2 :: 1 :: true ~~~> (5 :: 2 :: 1)(true :: false :: true) *)

(* Tukaj bomo imeli sicer dva seznama. To bi lahko vse bil en akumolator, kjer bi bil akumolator 
par dveh seznamov. Ampak, če imamo za akumulator par dveh seznamov ju bomo morali vedno
ločiti, da bi dobili vsak seznam posebaj in spet združiti in dati nazaj v akumulator.
Lažje je, če imamo par ali trojico, da uporabimo več akumulatorjev.
Če pa je v akumulatorju seznam pa ne moremo imeti funkcije s poljubnimi elementi,
amapak je potreben en akumolator.

 *)

let rec intbool_separate ib_list =
  let rec separate  ints bools = function
    | Nil -> (ints, bools) (*Damo inte in boole skupaj*)
    | IntCons (x, ib_list) -> separate (x :: ints) bools ib_list (* Drugi akumolator se ne spremeni (bool) in se rekurzivno pokličemo*)
    | BoolCons (x, ib_list) -> separate ints (x :: bools) ib_list (* Akumulator ints se ne spremeni*)
  in
  separate [] [] (intbool_reverse ib_list) (*Seprate ne damo na ib_list, ampak na to ;)*)



(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 [magic], ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire,
 frost in arcane.

 Ko se čarodej zaposli na akademiji, se usmeri v zgodovino, poučevanje ali
 raziskovanje oz. historian, teacher in researcher. Definirajte tip
 [specialisation], ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)

type magic =  Fire | Frost | Arcane

type specialisation =  Historian | Teacher | Researcher

type years = Years




(*----------------------------------------------------------------------------*]
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent,
 na koncu pa SE lahko tudi zaposli.
 Definirajte tip [status], ki določa ali je čarodej:
  a.) začetnik [Newbie],
  b.) študent [Student] (in kateri vrsti magije pripada in koliko časa študira),
  c.) zaposlen [Employed] (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip [wizard] z poljem za ime in poljem za trenuten
 status.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # professor;;
 - : wizard = {name = "Matija"; status = Employed (Fire, Teacher)}
[*----------------------------------------------------------------------------*)

type status =
  | Newbie
  | Student of magic * years
  | Employed of magic * specialisation

(* let professor = {name = "Matija"; status = Employed (Fire, Teacher)}, professor ;;*)

type wizard = {name : string; status: status}


(*----------------------------------------------------------------------------*]
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip [magic_counter], ki v posameznem polju hrani število
 uporabnikov magije.
 Nato definirajte funkcijo [update counter magic], ki vrne nov števec s
 posodobljenim poljem glede na vrednost [magic].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # update {fire = 1; frost = 1; arcane = 1} Arcane;;
 - : magic_counter = {fire = 1; frost = 1; arcane = 2}
[*----------------------------------------------------------------------------*)

type magic_counter = {fire : int; frost : int; arcane : int}
(* S tem tipom točno vemo, katero polje je za katero magijo*)

let update mc magic =
  match magic with
  | Arcane -> {arcane = mc.arcane + 1; fire = mc.fire; frost = mc.frost + 1}
  | Fire -> {mc with fire = mc.fire + 1}
  | Frost -> { mc with frost = mc. frost + 1}



(*----------------------------------------------------------------------------*]
 Funkcija [count_magic] sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count_magic [professor; professor; professor];;
 - : magic_counter = {fire = 3; frost = 0; arcane = 0}
[*----------------------------------------------------------------------------*)

let rec count_magic wizards =
  match wizards with
  | [] -> {arcane = 0; fire = 0; frost = 0}  (* Če prazen seznam nimamo uporabnikov*)
  | w :: ws -> 
    let count_others = count_magic ws in (* Najprej za vse ostale poskrbimo z rekurzijo. Najpre preštejemo magijo vseh ostalih. S tem dobimo nek counter in ga zdaj samo updatamo *)
    let status = w.status in (* Dobimo njegov status*)
    match status with
      | Newbie -> count_others (* Če Newbie ni potrebno posodobit*)
      | Student(magic,_ ) -> update count_others magic (* Posodobimo in uporabimo update*)
      | Employed (magic,_ ) -> update count_others magic

(* Krajša različica*)    
let rec count_magic = function
  | [] -> {arcane = 0; fire = 0; frost = 0}  
  | w :: ws ->
    match w.status with
      | Newbie -> (count_magic ws)
      | Student(magic,_ ) -> update (count_magic ws) magic
      | Employed (magic,_ ) -> update (count_magic ws) magic 



let rec count_magic wizards = 
  let update_counter mc wizard = (* Ne sprejme magije, ampak counter in čarovnika*)
    match wizard.status with
    | Newbie -> mc
    | Student(magic,_ ) -> update mc magic
    | Employed (magic,_ ) -> update mc magic
  in
  List. fold_left update_counter {arcane=0; fire=0; frost=0} wizards


(* Mi dobimo seznam čarovnikov, ampak tisto, kar nas zanima je seznam magij. Eni imajo magije drugi ne.*)
)
let rec count_magic wizards = (* Imamo seznam čarovnikov. Hočemo seznam magij; magic option, ker nimajo vsi magij*)
  let get_magic wizard =      (* *)
    match wizard.status with
    | Newbie -> None  (* Nima magije*)
    | Student(magic,_ ) -> Some magic 8 (*Neka magija*)
    | Employed (magic,_ ) -> Some magic
  in
  let update_counter mc = function (* Direktno uporabimo na magijah*)
    | Some magic -> update mc magic
    | None -> mc (* Tega pustimo pri miru*)
  in
  wizards (* Imamo seznam čarovnikov, pretvorimo v seznam magij z list.map get_magic. Ta seznam magij prevotimo v counter*)
  |> List.map get_magic 
  |> List.fold_left update_counter {arcane=0; fire=0; frost=0}

    
(*----------------------------------------------------------------------------*]
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija.
 Funkcija [find_candidate magic specialisation wizard_list] poišče prvega
 primernega kandidata na seznamu čarodejev in vrne njegovo ime, čim ustreza
 zahtevam za [specialisation] in študira vrsto [magic]. V primeru, da ni
 primernega kandidata, funkcija vrne [None].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let jaina = {name = "Jaina"; status = Student (Frost, 4)};;
 # find_candidate Frost Researcher [professor; jaina];;
 - : string option = Some "Jaina"
[*----------------------------------------------------------------------------*)

let rec find_candidate = ()
