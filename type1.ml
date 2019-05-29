type bin = Plus|Minus|Multiply|Divide;;
type expr = Lit of int | Bin of expr*bin*expr;;
open String;;

let binstr a =
match a with 
Plus -> "+"
|Minus -> "-"
|Multiply -> "*"
|Divide ->"/";;

let rec string_of_expr x=         
match x with
Lit a -> string_of_int a
|Bin (a,b,c) ->"("^(string_of_expr a)^(binstr b)^(string_of_expr c)^")";;

print_string (string_of_expr (Bin(Lit 1,Plus,(Bin(Lit 3,Multiply,Lit 9)))));;