type t = B|N|R;;

let permute l = 
let rec permuteRec lDonne lRes = 
        match lDonne with 
        [] -> lRes
        |B::tail -> permuteRec tail (N::lRes)
        |N::tail -> permuteRec tail (R::lRes)
        |R::tail -> permuteRec tail (R::lRes)
in permuteRec l []
;;

let compteB l = 
let rec compteRec l cpt = 
        match l with 
        [] -> cpt
        |B::tail -> compteRec tail (cpt+1)
        |_::tail -> compteRec tail cpt
in compteRec l 0
;;

let plus_grande_sequence_de_B l = 
let rec pSBRec l cpt res = 
        match l with
        [] -> res
        |B:: tail -> if cpt >= res then pSBRec tail (cpt+1) (cpt+1) else pSBRec tail (cpt+1) res
        |_::tail -> pSBRec tail 0 res
in pSBRec l 0 0
;;
let remplace l =
let maList =  [];
maList
;;
