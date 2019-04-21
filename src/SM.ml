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
(* conditional jump                *) | CJMP  of string * string
(* begins procedure definition     *) | BEGIN of string list * string list
(* end procedure definition        *) | END
(* calls a procedure               *) | CALL  of string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         
let rec eval env conf prog =
  let (control_st, st, (s, i, o)) = conf in
  match prog with
    | []                       -> conf
    | BINOP op           :: ps -> let (x :: y :: rest) = st in
                                  let res = Expr.eval s (Binop (op, Const y, Const x)) in
                                  eval env (control_st, res :: rest, (s, i, o)) ps
    | CONST c            :: ps -> eval env (control_st, c :: st, (s, i, o)) ps
    | READ               :: ps -> let (head :: rest) = i in
                                  eval env (control_st, head :: st, (s, rest, o)) ps
    | WRITE              :: ps -> let (head :: rest) = st in
                                  eval env (control_st, rest, (s, i, o @ [head])) ps
    | LD x               :: ps -> eval env (control_st, State.eval s x :: st, (s, i, o)) ps
    | ST x               :: ps -> let (head :: rest) = st in
                                  eval env (control_st, rest, (State.update x head s, i, o)) ps
    | LABEL l            :: ps -> eval env conf ps
    | JMP l              :: _  -> eval env conf (env#labeled l)
    | CJMP (z, l)        :: ps -> let (head :: rest) = st in
                                  if z = "z" && head = 0 || z = "nz" && head != 0
                                  then eval env (control_st, rest, (s, i, o)) (env#labeled l)
                                  else eval env (control_st, rest, (s, i, o)) ps
    | BEGIN (args, vars) :: ps -> let state = State.push_scope s (args @ vars) in
                                  let s', st' = List.fold_left
                                    (fun (s, v :: st) name -> (State.update name v s, st)) (state, st) args in
                                  eval env (control_st, st', (s', i, o)) ps
    | END                :: _  -> (
                                     match control_st with
                                       | (old_prg, old_s) :: rest -> eval env (rest, st, (State.drop_scope s old_s, i, o)) old_prg
                                       | _ -> conf
                                  )
    | CALL f             :: ps -> eval env ((ps, s) :: control_st, st, (s, i, o)) (env#labeled f)

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
  let (_, _, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [])) p in o

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
    | Stmt.Call (f, args)  -> List.concat (List.map compile_expr (List.rev args)) @ [CALL f], false

let rec compile_prg p =
    let lb = label#generate_next in
    let prg, used = compile_impl p lb in
    prg @ (if used then [LABEL lb] else [])

let rec compile_defs defs =
    List.fold_left
      (fun (p) (n, (args, vars, body)) -> let body = compile_prg body in
                                          p @ [LABEL n] @ [BEGIN (args, vars)] @ body @ [END]) ([]) defs

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile (defs, main) =
  let main = compile_prg main in
  let defs = compile_defs defs in
  main @ [END] @ defs
