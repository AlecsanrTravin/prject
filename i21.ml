type tree = Val of int | Mul of tree * tree | Sum of tree * tree ;;

let a = read_line () ;;

let parse s =
    let rec parse_s pos =
        let ( t1 , m ) = parse_m pos in 
        if s.[ m ] = '+' then 
        let ( t2 , m2 ) = parse_s ( m + 1 ) in 
        ( Sum ( t1 , t2 ) , m2 ) 
        else 
        ( t1 , (m +1))
    and parse_m pos =
        let ( t , m ) = parse_t pos in
        if s.[ m ] = '*' then
        let ( t2 , m2 ) = parse_m ( m + 1 ) in
        ( Mul ( t , t2 ) , m2 )
        else
        ( t , (m +1))
    and parse_t m =
        if s.[ m ] >= '0' && s.[m] <= '9' then (Val (int_of_char s.[m] - int_of_char '0'), ( m + 1 ) )
        else failwith "Unexpected"
    in parse_s 0;;

parse a;;

