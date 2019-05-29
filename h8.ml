type beackets = B of beackets list ;;

let s = "(())((()())()(()));" ;;

let rec print_brackets b =
List.iter (fun x -> match x with 
B x -> print_string "(" ; print_brackets x ; print_string ")"
) b ;;

let rec parce i =
if s.[i] = '(' then
let (new_i, t) = parce (i+1) in
if s.[new_i] <> ')' then failwith ") expected "; 
let (new_i, tt) = parce (new_i + 1) in 
(new_i , B t :: tt)
else
(i, []) ;;

let (_, b) = parce 0 ;;

print_brackets b ;;
