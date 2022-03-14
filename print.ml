open Functions
open Syntaxe

(* Prints an operator *)
let print_op (op) =
  match op with 
  | Add -> print_string "+"
  | Sub -> print_string "-"
  | Mul -> print_string "*"
  | Div -> print_string "/"
  | Mod -> print_string "%"

(* Prints an expression *)
let rec print_expr (exp) =
  match exp with 
  | Num (x) ->  print_string " ";
                print_int x;

  | Var (x) ->  print_string " ";
                print_string x;

  | Op  (op, exp1, exp2) -> print_string " ";
                            print_op op;
                            print_expr exp1;
                            print_expr exp2

(* Prints a comparator *)
let print_comp (comp) =
  match comp with
  | Eq -> print_string "="
  | Ne -> print_string "<>"
  | Lt -> print_string "<"
  | Le -> print_string "<="
  | Gt -> print_string ">" 
  | Ge -> print_string ">="


(* Prints a condition *)
let print_cond (cond) =
  match cond with 
  | (x,y,z) ->  print_expr x;
                print_string " ";
                print_comp y;
                print_expr z

(* Prints the indentation of a line *)
let rec print_indentation (indent) (pos) (txt) =
    if (pos != indent) then
      print_indentation indent (pos+1) (" "^txt)
    else 
      txt

(* Prints a block of instructions *)
let rec print_block (block) (indent) (pos)= 
    if (pos <= (max_pos block 0)) then
        (print_string (print_indentation indent 0 "");
        let inst = search_block pos block in
        match inst with
        | Set(x,y) -> print_string x;
                      print_string " :=";
                      print_expr y;
                      print_string "\n";
                      print_block block indent (pos+1)

        | Read(x) ->  print_string "READ ";
                      print_string x;
                      print_string "\n";
                      print_block block indent (pos+1)

        | Print(x) -> print_string "PRINT";
                      print_expr x;
                      print_string "\n";
                      print_block block indent (pos+1)

        | If(c,b1,b2) ->  print_string "IF";
                          print_cond c;
                          print_string "\n";
                          print_block b1 (indent + 2) (pos+1);
                          if (b2 = []) then
                            print_block block indent ((max_pos b1 0) +1)
                          else
                             (print_string ((print_indentation indent 0 "")^"ELSE\n");
                              print_block b2 (indent + 2) ((max_pos b1 0)+1);
                              print_block block indent ((max_pos b2 0) +1))

        | While(c,b) -> print_string "WHILE";
                        print_cond c;
                        print_string "\n";
                        print_block b (indent + 2) (pos+1);
                        print_block block indent ((max_pos b 0) + 1)
        )

(* Main method : Prints a program*)
let print_polish (p) =  
  let min = min_instr p (List.length p) in
  print_block p 0 min
