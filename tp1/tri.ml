let couper l = 
        let rec coupeRec l res1 res2 =
               match l with 
                [] -> res1 res2
                |h1::[] -> if (List.length res1) >= (List.length res2) then coupeRec [] res1 (h1::res2) else coupeRec [] (h1::res1) res2
                |h1::h2::tail -> coupeRec tail (h1::res1) (h2::res2)
        in coupeRec l [] []
;;

let affiche_liste_entiers l = 
        let rec affiche l1 = 
                match l1 with 
                [] -> ""
                |h::tail -> print_int h ; affiche tail
        in affiche l;;
                   
