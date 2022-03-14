open Syntaxe
open Functions


(*Prints a set's elements*)
let rec print_Set (elts) =
  match elts with
  |[] -> print_newline()
  |elt::l ->  (print_string elt; print_string " "; print_Set l;)
;;

(*Main function*)
let vars_polish (p) =
  let vars_init = Names.empty in
  let vars_non_init = Names.empty in

  (*Analyses a block or a program*)
  let rec vars (p) ((init) ,(non_init)) (pos) =
    
    let analyse_instr (instr) ((init) ,(non_init)) =
      let rec analyse_expr (expr) ((init) ,(non_init)) =         
        match expr with
        |Num(x) -> (init,non_init)
        |Var(name) -> if Names.mem name init then
                        (init,non_init)
                      else
                        (Names.add name init,Names.add name non_init)
      
        |Op(op,expr1,expr2) ->  union_Set_expr expr1 expr2 (init,non_init)
      
      
      (*Unites 2 expressions' respective sets*)
      and union_Set_expr (expr1) (expr2) (init, non_init) =
        match analyse_expr expr1 (init,non_init) with
                                    |(init1,non_init1) -> match analyse_expr expr2 (init,non_init) with
                                                          |(init2,non_init2) -> (Names.union init1 init2, Names.union non_init1 non_init2)
      
      (*Analyses a condition*)
      and analyse_cond (cond) ((init), (non_init)) =
        match cond with
        (expr1,comp,expr2) -> union_Set_expr expr1 expr2 (init, non_init)

      in

      match instr with
      |Set(x,y) -> (match analyse_expr y (init,non_init) with
                   |(initN,non_initN) -> (Names.add x initN, non_initN))

      |Read(name) -> (Names.add name init, non_init)

      |If(cond,b1,b2) -> (match vars b1 (init,non_init) (pos + 1) with
                         |(initB1,non_initB1) -> match vars b2 (init,non_init) ((max_pos b1 0) + 1) with
                                                 |(initB2,non_initB2) -> let initN = Names.union initB1 initB2 in 
                                                                         let non_initN = Names.union (Names.union non_initB1 non_initB2) (Names.union (Names.diff initB2 initB1)(Names.diff initB1 initB2)) in
                                                                         match analyse_cond cond (init,non_init) with
                                                                         |(initCond, non_initCond) -> (Names.union initN initCond, Names.union non_initN non_initCond))
      
      |Print(expr) -> analyse_expr expr (init, non_init)

      |While(cond,block) -> match vars block (init,non_init) (pos + 1) with
                            |(initN, non_initN) -> match analyse_cond cond (init, non_init) with
                                                   |(initCond, non_initCond) -> let  diffInitBlock = Names.diff init initN in
                                                                                (Names.union initN initCond, (Names.union (Names.union non_initN diffInitBlock) non_initCond))
    in 
    if (pos <= (max_pos p 0)) then
      let newSets = analyse_instr (search_block pos p) (init, non_init) in
      vars p newSets (next_pos pos ((max_pos p 0) +1) p)
    else
      (init, non_init)
    
  in

  match vars p (vars_init, vars_non_init) 0 with
  |(init, non_init) -> print_Set (Names.elements init);
                       print_Set (Names.elements non_init);
;;