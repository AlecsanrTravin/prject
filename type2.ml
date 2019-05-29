type bin = Plus|Minus|Multiply|Divide;;
type expr = Lit of int | Bin of expr*bin*expr;;


let binfun a b c =
match b with 
Plus -> a +c 
|Minus -> a-c
|Multiply ->  a*c
|Divide ->a/c;;

let rec int_of_expr x =
match x with
Lit a -> a
|Bin (a,b,c) -> binfun (int_of_expr a) b (int_of_expr c);;

print_int (int_of_expr (Bin(Lit 1,Plus,(Bin(Lit 3,Multiply,Lit 9)))));;