open GT       
open Language
       
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
type config = int list * Stmt.config

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

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

let rec compile_expr e =
  match e with
    | Expr.Const n          -> [CONST n]
    | Expr.Var v            -> [LD v]
    | Expr.Binop (op, x, y) -> (compile_expr x) @ (compile_expr y) @ [BINOP op]

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compile t =
  match t with
    | Stmt.Read v        -> [READ; ST v]
    | Stmt.Write e       -> (compile_expr e) @ [WRITE]
    | Stmt.Assign (v, e) -> (compile_expr e) @ [ST v]
    | Stmt.Seq (s1, s2)  -> (compile s1) @ (compile s2)
