open GT       
open Syntax
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

let eval_op (stack, st_conf) op =
  let (s, i, o) = st_conf in
  match op with
    | BINOP op -> let (x :: y :: rest) = stack in
                  let res = Expr.Binop (op, Const y, Const x) in
                  (Expr.eval s res :: rest, st_conf)
    | CONST n  -> (n :: stack, st_conf)
    | READ     -> let (x :: rest) = i in
                  (x :: stack, (s, rest, o))
    | WRITE    -> let (x :: rest) = stack in
                  (rest, (s, i, o @ [x]))
    | LD v     -> (s v :: stack, st_conf)
    | ST v     -> let (x :: rest) = stack in
                  (rest, (Expr.update v x s, i, o))

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let rec eval conf prg =
  match prg with
    | []      -> conf
    | x :: xs -> eval (eval_op conf x) xs

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

let rec compile_expr e =
  match e with
    | Syntax.Expr.Const n          -> [CONST n]
    | Syntax.Expr.Var v            -> [LD v]
    | Syntax.Expr.Binop (op, x, y) -> (compile_expr x) @ (compile_expr y) @ [BINOP op]

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compile t =
  match t with
    | Syntax.Stmt.Read v        -> [READ; ST v]
    | Syntax.Stmt.Write e       -> (compile_expr e) @ [WRITE]
    | Syntax.Stmt.Assign (v, e) -> (compile_expr e) @ [ST v]
    | Syntax.Stmt.Seq (s1, s2)  -> (compile s1) @ (compile s2)