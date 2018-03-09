(*--------------------EXCEPTIONS--------------------*)


(*Town exceptions*)
exception Invalid_Town
exception Only_One_Town
exception Town_Not_Connect
(*End Town exceptions*)

(*Create command exceptions*)
exception Create_Command_Wrong_Number_Of_Argument
exception Create_Command_Bad_Day_Format
exception Create_Command_Bad_Hour_Format
(*End Create command exceptions*)


(*----------------END-EXCEPTIONS--------------------*)



(*----------------NEW-TYPES-------------------------*)


(*Define new types*)
type jour = (int * int * int)
type heure = (int * int)
type distance = (string * string * int)
type date = (jour * heure)
type etape = (string * date * date)
type train = (string * int * etape list)
(*End Define new types*)

(*Create types*)
let create_etape ville date_a date_d = (ville, date_a, date_d)
let create_date jour heure = (jour, heure)
let create_jour jour mois annee = (jour, mois, annee)
let create_heure heure minute = (heure, minute)
let create_def_date = ((0, 0, 0), (0, 0))
(*End Create types*)

(*Get types*)
let get_ville (v, _, _) = v

let get_jour_a (_, ((j, _, _), _), _) = j
let get_mois_a (_, ((_, m, _), _), _) = m
let get_annee_a (_, ((_, _, a), _), _) = a
let get_heure_a (_, (_, (h, _)), _) = h
let get_minute_a (_, (_, (_, m)), _) = m

let get_jour_d (_, _, ((j, _, _), _)) = j
let get_mois_d (_, _, ((_, m, _), _)) = m
let get_annee_d (_, _, ((_, _, a), _)) = a
let get_heure_d (_, _, (_, (h, _))) = h
let get_minute_d (_, _, (_, (_, m))) = m

let get_dist (_, _, d) = d

let add_date ((j, m, a), (h, min)) n_h n_m =
	if (n_m + min >= 60)
		then ((j, m, a), (h + n_h + 1, (n_m + min) mod 60))
	else ((j, m, a), ((h + n_h), (n_m + min)))

let calc_date date vit km =
  let new_heure = km / vit
  and new_min = int_of_float (((float_of_int vit) /. 60.0) *. float_of_int (km mod vit))
  in
  add_date date new_heure new_min
(*End Get types*)


(*------------END-NEW-TYPES-------------------------*)



(*-------------------COMMAND------------------------*)


(*Sub parsing for Create command*)
let get_day string =
  let list = Str.split_delim (Str.regexp "-") string in
  if ((List.length list) != 3) then
    raise Create_Command_Bad_Day_Format
  else
  create_jour (int_of_string(List.nth list 0)) (int_of_string(List.nth list 1)) (int_of_string(List.nth list 2))

let get_hour string =
  let list = Str.split_delim (Str.regexp ":") string in
  if ((List.length list) != 2) then
    raise Create_Command_Bad_Hour_Format
  else
  create_heure (int_of_string(List.nth list 0)) (int_of_string(List.nth list 1))

let get_city string = Str.split_delim (Str.regexp ",") string
(*End Sub parsing for Create command*)

(*Get parsed Create command*)
let get_info string =
  let word_list = Str.split_delim (Str.regexp " ") string in
  if ((List.length word_list) != 5) then
    raise Create_Command_Wrong_Number_Of_Argument
  else
    let type_of_train = (List.nth word_list 1) in
    let day = (get_day (List.nth word_list 2)) in
    let hour =  (get_hour (List.nth word_list 3)) in
    let city_list = (get_city (List.nth word_list 4)) in
    (type_of_train, (day, hour), city_list)
(*End Get parsed Create command*)

(*Delete command*)
let delete_train list str =
  List.filter (fun (t, n, e) -> if (String.equal (t^(string_of_int n)) str) == true then false else true) list
(*End Delete command*)


(*--------------END--COMMAND------------------------*)



(*-------------------PRINT--------------------------*)


(*Print functions*)
let print_etape etape =
  begin
    print_string (get_ville etape);
    print_string " (";
    if get_jour_a etape != 0
    then Printf.printf("%02d-%02d-%04d,%02d:%02d") (get_jour_a etape)
      (get_mois_a etape) (get_annee_a etape)
      (get_heure_a etape) (get_minute_a etape)
    else print_string ",";
    print_string ") (";
    if get_jour_d etape != 0
    then Printf.printf("%02d-%02d-%04d,%02d:%02d") (get_jour_d etape)
      (get_mois_d etape) (get_annee_d etape)
      (get_heure_d etape) (get_minute_d etape)
    else print_string ",";
    print_string ")\n"
  end

let rec print_list_etape =
	function
	| [] -> ()
	| a::b -> print_etape a; print_list_etape b

let rec print_train =
function
| [] -> ()
| (name, num, etape_list)::b -> Printf.printf("%s %04d\n") name num; print_list_etape etape_list; print_train b
(*End Print functions*)


(*---------------END-PRINT--------------------------*)



(*-------------------MODULES--------------------------*)


(*Module Declarations*)
module type TRAIN =
sig
	val t : string
	val vitesse : int
	val dist : distance list
	val ville : string list
	val create_etape_list : date -> string list -> bool -> etape list
	val new_date : date -> string -> string -> date
end

module Tgv : TRAIN =
struct
	let t = "TGV"
	let vitesse = 230
	let ville = ["Brest"; "Le Havre"; "Lille" ; "Paris"
							; "Strasbourg"; "Nancy"; "Dijon"; "Lyon"; "Nice"; "Marseille"
							;"Montpellier"; "Perpignan"; "Bordeaux"; "Nantes"; "Avignon"
							; "Rennes"; "Biarritz"; "Toulouse"; "Le Mans"]
	let dist = [("Paris", "Lyon", 427); ("Brest", "Rennes", 248); ("Le Mans", "Paris", 201); ("Lyon", "Marseille", 325)
		; ("Dijon", "Lyon", 192); ("Paris", "Le Havre", 230); ("Rennes", "Le Mans", 163); ("Le Mans", "Nantes", 183)
		; ("Paris", "Lille", 225); ("Paris", "Bordeaux", 568); ("Nancy", "Strasbourg", 149); ("Paris", "Strasbourg", 449)
		; ("Paris", "Nancy", 327); ("Dijon", "Strasbourg", 309); ("Toulouse", "Bordeaux", 256); ("Montpellier", "Toulouse", 248)
		; ("Dijon", "Nancy", 226); ("Marseille", "Montpellier", 176)]
	let new_date date ville_d ville_a =
	try
	let p = List.find (fun (v1, v2, d) -> (v1, v2, d) = (ville_a, ville_d, d) || (v1, v2, d) = (ville_d, ville_a, d)) dist
		in
		calc_date date vitesse (get_dist p)
	with
	| Not_found -> raise Town_Not_Connect
	let rec create_etape_list date villes first =
	if (List.mem (List.hd villes) ville) != true
	then raise Invalid_Town
	else if (List.length villes == 1 && first == true)
	then raise Only_One_Town
	else if (List.length villes == 1)
	then [create_etape (List.hd villes) date create_def_date]
	else if (first == true)
	then List.append [(create_etape (List.hd villes) create_def_date date)] (create_etape_list (new_date date (List.hd villes) (List.hd (List.tl villes))) (List.tl villes) false)
	else List.append [(create_etape (List.hd villes) date (add_date date 0 10))] (create_etape_list (new_date (add_date date 0 10) (List.hd villes) (List.hd (List.tl villes))) (List.tl villes) false)
end

module Eurostar : TRAIN =
struct
	let t = "Eurostar"
	let vitesse = 160
	let ville = ["Paris";"Lille";"Brussel";"London"]
	let dist = [("Paris", "Lille", 225)
		; ("Lille", "Brussel", 106)
		; ("Lille", "London", 269)]
	let new_date date ville_d ville_a =
	let p = List.find (fun (v1, v2, d) -> (v1, v2, d) = (ville_a, ville_d, d) || (v1, v2, d) = (ville_d, ville_a, d)) dist
		in
		calc_date date vitesse (get_dist p)
	let rec create_etape_list date villes first =
	if (List.mem (List.hd villes) ville) != true
	then raise Invalid_Town
	else if (List.length villes == 1 && first == true)
	then raise Only_One_Town
	else if (List.length villes == 1)
	then [create_etape (List.hd villes) date create_def_date]
	else if (first == true)
	then List.append [(create_etape (List.hd villes) create_def_date date)] (create_etape_list (new_date date (List.hd villes) (List.hd (List.tl villes))) (List.tl villes) false)
	else List.append [(create_etape (List.hd villes) date (add_date date 0 10))] (create_etape_list (new_date (add_date date 0 10) (List.hd villes) (List.hd (List.tl villes))) (List.tl villes) false)
end

module Thalys : TRAIN =
struct
	let t = "Thalys"
	let vitesse = 210
	let ville = ["Paris";"Lille";"Brussel";"Liege";"Amsterdam";"Cologne";"Essen"]
	let dist = [("Paris", "Lille", 225); ("Cologne", "Essen", 81); ("Brussel", "Liege", 104)
		; ("Lille", "Brussel", 106); ("Brussel", "Amsterdam", 211); ("Liege", "Cologne", 118)]
	let new_date date ville_d ville_a =
	let p = List.find (fun (v1, v2, d) -> (v1, v2, d) = (ville_a, ville_d, d) || (v1, v2, d) = (ville_d, ville_a, d)) dist
		in
		calc_date date vitesse (get_dist p)
	let rec create_etape_list date villes first =
	if (List.mem (List.hd villes) ville) != true
	then raise Invalid_Town
	else if (List.length villes == 1 && first == true)
	then raise Only_One_Town
	else if (List.length villes == 1)
	then [create_etape (List.hd villes) date create_def_date]
	else if (first == true)
	then List.append [(create_etape (List.hd villes) create_def_date date)] (create_etape_list (new_date date (List.hd villes) (List.hd (List.tl villes))) (List.tl villes) false)
	else List.append [(create_etape (List.hd villes) date (add_date date 0 10))] (create_etape_list (new_date (add_date date 0 10) (List.hd villes) (List.hd (List.tl villes))) (List.tl villes) false)
end
(*End Module Declarations*)


(*--------------END--MODULES--------------------------*)



(*-------------------FUNCTOR--------------------------*)


module type CREATE_TRAIN =
sig
	val t : date -> string list -> int -> train
end

module type MAKE_TRAIN = functor (Train : TRAIN) -> CREATE_TRAIN

module Make_Train : MAKE_TRAIN = functor (Train : TRAIN) ->
struct
  let t date villes rdm = Printf.printf("Trip created: %s %04d\n") Train.t rdm;(Train.t, rdm, (Train.create_etape_list date villes true))
end

module Make_TGV = Make_Train (Tgv)
module Make_Eurostar = Make_Train (Eurostar)
module Make_Thalys = Make_Train (Thalys)


(*---------------END-FUNCTOR--------------------------*)
