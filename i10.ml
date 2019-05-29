let rec print_int_list l =
 match l with
 hd :: tl -> print_int hd ; print_string " " ; print_int_list tl
 |_-> print_string " " ;;

let a = [| 1 ; 4 ; 7 ; 8 ; 3 ; 75 ; 9876 ; 57 ; 9 ; 0 ; 15 ; 24 ; 879 ; 228 ; 689 ; 700 ; 89 |] ;;

 let swap i j a = let t = a.(i) in a.(i) <- a.(j); a.(j) <- t ;;

 let rec sort_2_array a =
  for i = 0 to ( Array.length a -1 ) / 2 do
   for j = 0 to (( Array.length a ) - 2 - ( 2 * i ) )  do
    if  a.((j )) <  a.((j  + 1)) then swap j (j + 1) a else ()
   done ;
  done ; a
;;

 print_int_list (Array.to_list (  sort_2_array a )) ;;