let rec main list =
  try
    let line = read_line ()
    in
    if String.length line == 4 && (String.compare line "quit") == 0
    then exit(0)
    else if String.length line == 4 && (String.compare (String.sub line 0 4) "list") == 0
    then Trip.print_train list
    else if String.length line > 7 && (String.compare (String.sub line 0 7) "create ") == 0
    then let (t, date, s_list) = Trip.get_info line
	 in
	 match t with
	 | "TGV" -> main (List.append list [(Trip.Make_TGV.t date s_list ((Random.int 9000) + 1000))])
	 | "Eurostar" -> main (List.append list [(Trip.Make_Eurostar.t date s_list ((Random.int 9000) + 1000))])
	 | "Thalys" -> main (List.append list [(Trip.Make_Thalys.t date s_list ((Random.int 9000) + 1000))])
	 | _ -> print_string (t^" didn't exist\n"); main list
    else if String.length line > 7 && (String.compare (String.sub line 0 7) "delete ") == 0
    then main (Trip.delete_train list (String.sub line 7 ((String.length line) - 7)))
    else print_string "Wrong Command !\n" ; main list
  with
  | End_of_file -> exit (0)
  | Trip.Invalid_Town -> print_string "One city didn't exist\n" ; main list
  | Trip.Only_One_Town -> print_string "Need at least 2 cities\n" ; main list
  | Trip.Town_Not_Connect -> print_string "Some cities are not connected\n"; main list
in main []
