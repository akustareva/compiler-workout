open GT  
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         
let rec eval env conf prog =
  let (stack, (st, inConf, outConf)) = conf in
  match prog with
    | []                -> conf
    | BINOP op    :: ps -> let (x :: y :: rest) = stack in
                           let res = Expr.apply_op op y x in
                           eval env (res :: rest, (st, inConf, outConf)) ps
    | CONST c     :: ps -> eval env (c :: stack, (st, inConf, outConf)) ps
    | READ        :: ps -> let (head :: rest) = inConf in
                           eval env (head :: stack, (st, rest, outConf)) ps
    | WRITE       :: ps -> let (head :: rest) = stack in
                           eval env (rest, (st, inConf, outConf @ [head])) ps
    | LD x        :: ps -> eval env (st x :: stack, (st, inConf, outConf)) ps
    | ST x        :: ps -> let (head :: rest) = stack in
                           eval env (rest, (Expr.update x head st, inConf, outConf)) ps
    | LABEL l     :: ps -> eval env conf ps
    | JMP l       :: _  -> eval env conf (env#labeled l)
    | CJMP (z, l) :: ps -> let (head :: rest) = stack in
                           if z = "z" && head = 0 || z = "nz" && head != 0
                           then eval env (rest, (st, inConf, outConf)) (env#labeled l)
                           else eval env (rest, (st, inConf, outConf)) ps

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o

let label =
  object(self)
    val mutable n = 0
    method generate_next = n <- n + 1; "lb" ^ string_of_int n
  end

let rec compile_expr e =
  match e with
    | Expr.Const c          -> [CONST c]
    | Expr.Var x            -> [LD x]
    | Expr.Binop (op, x, y) -> compile_expr x @ compile_expr y @ [BINOP op]

let rec compile_impl p lb =
  let jmp_by_cond cond lb = if cond then [] else [JMP lb] in
  match p with
    | Stmt.Read v          -> [READ; ST v], false
    | Stmt.Write e         -> compile_expr e @ [WRITE], false
    | Stmt.Assign (v, e)   -> compile_expr e @ [ST v], false
    | Stmt.Seq (e1, e2)    -> let lb' = label#generate_next in
                              let (p1, u1) = compile_impl e1 lb' in
                              let (p2, u2) = compile_impl e2 lb in
                              p1 @ (if u1 then [LABEL lb'] else []) @ p2, u2
    | Stmt.Skip            -> [], false
    | Stmt.If (e, e1, e2)  -> let lb' = label#generate_next in
                              let (p1, u1) = compile_impl e1 lb in
                              let (p2, u2) = compile_impl e2 lb in
                              compile_expr e @ [CJMP ("z", lb')] @ p1 @ jmp_by_cond u1 lb @ [LABEL lb'] @ p2 @ jmp_by_cond u2 lb, true
    | Stmt.While (e1, e2)  -> let c_lb = label#generate_next in
                              let l_lb = label#generate_next in
                              let (p, _) = compile_impl e2 c_lb in
                              [JMP c_lb; LABEL l_lb] @ p @ [LABEL c_lb] @ compile_expr e1 @ [CJMP ("nz", l_lb)], false
    | Stmt.Repeat (e1, e2) -> let l_lb = label#generate_next in
                              let (p, _) = compile_impl e1 lb in
                              [LABEL l_lb] @ p @ compile_expr e2 @ [CJMP ("z", l_lb)], false

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile p =
  let lb = label#generate_next in
  let prg, u = compile_impl p lb in
  prg @ if u then [LABEL lb] else []
