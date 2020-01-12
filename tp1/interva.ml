type interval = { inf : int ; supp : int};;
let make_interval a b  = 
        if a <= b then 
                { inf = a ; supp = b} 
        else { inf = b ; supp=a} 
;;

let add i1 i2  = 
        make_interval (i1.inf+i2.inf) (i1.supp+i2.supp)
;;
let sub i1 i2 = 
        make_interval ( (min i1.inf i2.inf) - (max i1.supp i2.supp) ) ( (max i1.supp i2.supp) - (min i1.inf i2.inf))
;; 
let mul i1 i2 = 
        make_interval (i1.inf * i2.inf) (i1.supp*i2.supp)
;;

