let x = read_int () ;;

let rec print_1 a = if 
a = 1 then print_string "7"
else print_string " " ; (print_1 (a - 1))
;;

let rec print_strelocha x = 
if x = 0 then print_string "*\n"
else print_1 x ; print_string "*\n" ; (print_strelocha (x-1)) ;;

(*let rec printstrelocha st x = 
if st = x then print_1 x ; print_string "*\n"
else print_1 st ; print_string "*\n" ; (printstrelocha ( st + 1 ), x) ;;
*)
(*printstrelocha 0 x ;;*)
print_strelocha x ;;