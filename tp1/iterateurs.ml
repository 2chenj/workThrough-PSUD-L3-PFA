let affiche_liste_entiers l = 
        List.iter print_int l
;;

print_string"\n----------affiche_liste---------------\n";;

affiche_liste_entiers [1;2;3;4;5;6;7;8;9];;
let count cible l = 
        List.fold_left  (fun a b -> if cible = b then a+1 else a) 0 l
;;
print_string"\n----------count---------------\n";;
print_int (count 5 [1;2;3;4;5;5;5;7;8]);;

