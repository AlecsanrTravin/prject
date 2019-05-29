 let a = [| 1 ; 4 ; 7 ; 8 ; 3 ; 75 ; 9876 ; 57 ; 9 ; 0 ; 15 ; 24 ; 879 ; 228 ; 689 ; 700 ; 89 |] ;;

 let swap i j a = let t = a.(i) in a.(i) <- a.(j); a.(j) <- t ;;

 let rec sort_array a = 
  for i = 0 to Array.length a -1 do 
   for j = 0 to ((Array.length a ) - 2) do
    if  a.(j) <  a.(j+1) then swap j (j + 1) a else ()
   done ;
  done ; a
;;

 let rec print_int_list l =
 match l with
 hd :: tl -> print_int hd ; print_string " " ; print_int_list tl
 |_-> print_string " " ;;

 let rec perevorot nl l =
 match l with
 hd :: tl -> perevorot (hd :: nl) tl
 |_-> nl ;;

 print_int_list ( perevorot [] (Array.to_list ( sort_array a ))) ;;

 print_string "\n" ;;

 print_int_list (Array.to_list ( sort_array a )) ;;