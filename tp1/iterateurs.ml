let affiche_liste_entiers l = 
        List.iter print_int l
;;

print_string"\n----------affiche_liste---------------\n";;

affiche_liste_entiers [1;2;3;4;5;6;7;8;9];;
let count cible l = 
        List.fold_left  (fun a b -> if cible = b then a+1 else a) 0 l
;;
print_endline "\n----------count---------------\n";;
print_int (count 5 [1;2;3;4;5;5;5;7;8]);;

let rec funOuter l=
        let rec funInnerRec l2 acc = 
              match l2 with 
              [] -> acc
              |h::tail ->funInnerRec tail (h::acc)
        in funInnerRec l []
;;

let rec funrec l acc = 
        match l with 
        []-> acc 
        |h::tail -> funrec tail (h::acc)
;;

let flatten l =
        List.fold_right (fun l1 acc -> (funrec (List.rev l1) acc) ) l [];;
print_endline "\n" ;;
print_endline " affichage de la liste flatternisé \n";;
affiche_liste_entiers (flatten [[1;2];[];[3;4;5];[6;7;8;9]]);;
print_endline "\n flatterned list \n"
let fst_list l = 
        List.fold_right (fun cpl acc -> (fst cpl)::acc ) l [];;
affiche_liste_entiers (fst_list [(1,"a");(2,"b");(3,"c");(4,"d");(5,"e")]);;
