type bin = Plus|Minus|Multiply|Divide;;
type expr = Lit of int | Bin of expr*bin*expr;;

let rec fact_expr n c=
if n=c then Lit c
else if n > c then Bin (Lit c,Multiply,(fact_expr n (c+1))) else failwith "Impossible";;
              

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

print_string (string_of_expr (fact_expr (read_int()) 1));;