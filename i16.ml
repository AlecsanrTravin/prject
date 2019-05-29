type binop = Plus | Minus | Mul | Div ;;

type expr = Lit of int | Binop of expr * binop * expr ;;

let bin = Binop ( Lit 7 , Plus , Binop ( Binop ( Lit 4 , Plus , Lit 7 ) , Mul , Lit 4 ));;

let rec print_binop bin =
match bin with
Lit x -> string_of_int x
| Binop ( expr1 , Plus , expr2 ) -> "(" ^ ( print_binop expr1 ) ^ "+" ^ ( print_binop expr2 ) ^ ")"
| Binop ( expr1 , Minus , expr2 ) -> "(" ^ ( print_binop expr1 ) ^ "-" ^ ( print_binop expr2 ) ^ ")"
| Binop ( expr1 , Mul , expr2 ) -> "(" ^ ( print_binop expr1 ) ^ "*" ^ ( print_binop expr2 ) ^ ")"
| Binop ( expr1 , Div , expr2 ) -> "(" ^ ( print_binop expr1 ) ^ "/" ^ ( print_binop expr2 ) ^ ")"  ;;

let rec raschot bin =
match bin with
Lit x ->  x
| Binop ( expr1 , Plus , expr2 ) -> (  raschot expr1 ) + (  raschot expr2 ) 
| Binop ( expr1 , Minus , expr2 ) -> (  raschot expr1 ) - (  raschot expr2 ) 
| Binop ( expr1 , Mul , expr2 ) -> (  raschot expr1 ) * (  raschot expr2 ) 
| Binop ( expr1 , Div , expr2 ) -> (  raschot expr1 ) / (  raschot expr2 )  ;;

print_string (print_binop bin);;

print_string "\n" ;;

print_int ( raschot bin);;

