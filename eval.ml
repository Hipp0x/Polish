open Functions
open Syntaxe


(* Main method : Evaluates a program *)
let eval_polish (p) = 
let env = NameTable.empty in

(* Converts an expression into its value *)
let rec exprToVal (ex) (envir)  =
  match ex with
  | Num(x) -> x

  | Var(x) -> (try NameTable.find x envir with Not_found -> (print_string ("Variable "^x^" does not exist");
                                                             print_newline();
                                                             exit(1)))
  
  | Op(op,ex1,ex2) -> 
    match op with
    | Add -> (exprToVal ex1 envir) + (exprToVal ex2 envir) 
    | Sub -> (exprToVal ex1 envir) - (exprToVal ex2 envir)
    | Mul -> (exprToVal ex1 envir) * (exprToVal ex2 envir)

    | Div ->  if (exprToVal ex2 envir) = 0 then 
                (print_string "Division par 0";
                exit(1))
              else
                (exprToVal ex1 envir) / (exprToVal ex2 envir)

    | Mod ->  if (exprToVal ex2 envir) = 0 then 
                (print_string "Modulo par 0";
                exit(1))
              else
                (exprToVal ex1 envir) mod (exprToVal ex2 envir) 
in

(* Converts a condition into its boolean value *)  
let condToBool (condition) (envir) = 
  match condition with
  | (ex1, compar, ex2) -> match compar with
                          | Eq -> (exprToVal ex1 envir) = (exprToVal ex2 envir)
                          | Ne -> (exprToVal ex1 envir) <> (exprToVal ex2 envir)
                          | Lt -> (exprToVal ex1 envir) < (exprToVal ex2 envir)
                          | Le -> (exprToVal ex1 envir) <= (exprToVal ex2 envir)
                          | Gt -> (exprToVal ex1 envir) > (exprToVal ex2 envir)
                          | Ge -> (exprToVal ex1 envir) >= (exprToVal ex2 envir)
in

(* Runs through a list of instructions and evaluates each one *)
let rec eval (p) (envir) (pos) =

  (* Evaluates the given instruction with an environement *)
  let rec instr_eval (ins) (envir) =
  
    match ins with
    | Set(x,y) -> NameTable.add x (exprToVal y envir) envir

    | Read(name) -> (print_string (name ^ "?");
                    let value = read_int_opt() in

                    match value with
                    |None -> instr_eval ins envir
                    | _ -> NameTable.add name (Option.get value) envir )
    
    | If(cond,b1,b2) ->  if (condToBool cond envir) then
                          eval b1 envir (pos + 1)
                        else
                          if b2 != [] then
                            eval b2 envir ((max_pos b1 0) + 1)
                          else
                            envir

    | Print(ex) -> (print_int (exprToVal ex envir);
                    print_newline();
                    envir)

    |While(cond,block) -> if (condToBool cond envir) then
                            let newenvir = eval block envir (pos + 1) in
                            instr_eval (While(cond,block)) newenvir
                          else
                            envir

  in
  
  if (pos <= (max_pos p 0)) then
    let newenvir = instr_eval (search_block pos p) envir in
    eval p newenvir (next_pos pos ((max_pos p 0) +1) p)
  else
    envir
  
in

let finalenv = eval p env (min_instr p (List.length p))  in
print_string "FIN";
print_newline();;