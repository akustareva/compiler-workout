(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT
open List

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

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

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)                                                       
    let rec eval s e =
      match e with
        | Const c          -> c
        | Var x            -> s x
        | Binop (op, x, y) -> apply_op op (eval s x) (eval s y)

    let parse_binop op = ostap(- $(op)), (fun x y -> Binop (op, x, y))
    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (                                      
      expr:
        !(Util.expr
          (fun x -> x)
          (Array.map
            (fun (a, ops) -> a, List.map parse_binop ops)
            [|
              `Lefta, ["!!"];
              `Lefta, ["&&"];
              `Nona , ["<="; "<"; ">="; ">"; "=="; "!="];
              `Lefta, ["+"; "-"];
              `Lefta, ["*"; "/"; "%"];
            |]
          )
          primary
        );

      primary:
          x:IDENT   { Var x }
        | c:DECIMAL {Const c}
        | -"(" expr -")"
    )
    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval (s, i, o) t =
      match t with
        | Read v          -> (Expr.update v (hd i) s, tl i, o)
        | Write e         -> (s, i, o @ [Expr.eval s e])
        | Assign (v, e)   -> (Expr.update v (Expr.eval s e) s, i, o)
        | Seq (s1, s2)    -> let new_conf = eval (s, i, o) s1 in
                             eval new_conf s2
        | Skip            -> (s, i, o)
        | If (e1, e2, e3) -> let r = Expr.eval s e1 in
                             if r != 0 then eval (s, i, o) e2 else eval (s, i, o) e3
        | While (e1, e2)  -> let r = Expr.eval s e1 in
                             if r != 0 then eval (eval (s, i, o) e2) (While (e1, e2)) else (s, i, o)
        | Repeat (e1, e2) -> let (s', i', o') = eval (s, i, o) e1 in
                             let r = Expr.eval s' e2 in
                             if r != 0 then (s', i', o') else eval (s', i', o') t

                               
    (* Statement parser *)
    ostap (
      parse: st:stmt ";" rest:parse {Seq (st, rest)} | stmt;

      stmt:
          "read" "(" x:IDENT ")"                                          {Read x}
        | "write" "(" e:!(Expr.expr) ")"                                 {Write e}
        | x:IDENT ":=" e:!(Expr.expr)                                    {Assign (x, e)}
        | "skip"                                                          {Skip}
        | "if" s:parse_if                                                 {s}
        | "while" c:!(Expr.expr) "do" e:parse "od"                        {While (c, e)}
        | "repeat" e:parse "until" c:!(Expr.expr)                         {Repeat (e, c)}
        | "for" i:parse "," c:!(Expr.expr) "," it:parse "do" e:parse "od" {Seq (i, While (c, Seq(e, it)))}
        ;

      parse_if:
        c:!(Expr.expr) "then" e1:parse e2:parse_else {If (c, e1, e2)}
        ;

      parse_else:
          "fi"                {Skip}
        | "else" e:parse "fi" {e}
        | "elif" s:parse_if   {s}
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     
