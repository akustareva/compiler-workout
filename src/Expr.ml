(* Simple expressions: syntax and semantics *)

(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
             
(* The type for the expression. Note, in regular OCaml there is no "@type..." 
   notation, it came from GT. 
*)
@type expr =
  (* integer constant *) | Const of int
  (* variable         *) | Var   of string
  (* binary operator  *) | Binop of string * expr * expr with show

(* Available binary operators:
    !!                   --- disjunction
    &&                   --- conjunction
    ==, !=, <=, <, >=, > --- comparisons
    +, -                 --- addition, subtraction
    *, /, %              --- multiplication, division, reminder
*)

(* State: a partial map from variables to integer values. *)
type state = string -> int

(* Empty state: maps every variable into nothing. *)
let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

(* Update: non-destructively "modifies" the state s by binding the variable x 
   to value v and returns the new state.
*)
let update x v s = fun y -> if x = y then v else s y

(* An example of a non-trivial state: *)                                                   
let s = update "x" 1 @@ update "y" 2 @@ update "z" 3 @@ update "t" 4 empty

(* Some testing; comment this definition out when submitting the solution. *)
(* let _ =
  List.iter
    (fun x ->
       try  Printf.printf "%s=%d\n" x @@ s x
       with Failure s -> Printf.printf "%s\n" s
    ) ["x"; "a"; "y"; "z"; "t"; "b"] *)

let apply_op op =
  let apply_res_func f g x y = f (g x y) in
  let bool_to_int x          = if x then 1 else 0 in
  let int_to_bool x          = x <> 0 in
  let apply_logical_op f x y = f (int_to_bool x) (int_to_bool y) in

  match op with
    | "+"  -> (  +  )
    | "-"  -> (  -  )
    | "*"  -> (  *  )
    | "/"  -> (  /  )
    | "%"  -> ( mod )
    | "<"  -> apply_res_func bool_to_int ( <  )
    | ">"  -> apply_res_func bool_to_int ( >  )
    | "<=" -> apply_res_func bool_to_int ( <= )
    | ">=" -> apply_res_func bool_to_int ( >= )
    | "==" -> apply_res_func bool_to_int ( =  )
    | "!=" -> apply_res_func bool_to_int ( <> )
    | "&&" -> apply_res_func bool_to_int (apply_logical_op ( && ))
    | "!!" -> apply_res_func bool_to_int (apply_logical_op ( || ))
    | _    -> failwith "Unknown operator"

(* Expression evaluator

     val eval : state -> expr -> int
 
   Takes a state and an expression, and returns the value of the expression in 
   the given state.
*)
let rec eval s e =
  match e with
    | Const x          -> x
    | Var x            -> s x
    | Binop (op, x, y) -> apply_op op (eval s x) (eval s y)

