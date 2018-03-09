exception Invalid_Town
exception Only_One_Town
exception Town_Not_Connect

type jour
type heure
type distance
type date
type etape
type train

val create_etape : string -> date -> date -> etape
val create_date : jour -> heure -> date
val create_jour : int -> int -> int -> jour
val create_heure : int -> int -> heure
val create_def_date : date

val print_etape : etape -> unit

val get_ville : etape -> string

val get_jour_a : etape -> int
val get_mois_a : etape -> int
val get_annee_a : etape -> int
val get_heure_a : etape -> int
val get_minute_a : etape -> int

val get_jour_d : etape -> int
val get_mois_d : etape -> int
val get_annee_d : etape -> int
val get_heure_d : etape -> int
val get_minute_d : etape -> int

val get_dist : distance -> int

val add_date : date -> int -> int -> date
val calc_date : date -> int -> int -> date

val get_info : string -> (string * date * string list)

val print_train : train list -> unit
val print_list_etape : etape list -> unit

val delete_train : train list -> string -> train list

module type TRAIN =
sig
	val t : string
	val vitesse : int
	val dist : distance list
	val ville : string list
	val create_etape_list : date -> string list -> bool -> etape list
	val new_date : date -> string -> string -> date
end

module Tgv : TRAIN
module Eurostar : TRAIN
module Thalys : TRAIN

module type CREATE_TRAIN =
sig
	val t : date -> string list -> int -> train
end

module type MAKE_TRAIN = functor (Train : TRAIN) -> CREATE_TRAIN
module Make_Train : MAKE_TRAIN
module Make_TGV : CREATE_TRAIN
module Make_Eurostar : CREATE_TRAIN
module Make_Thalys : CREATE_TRAIN
