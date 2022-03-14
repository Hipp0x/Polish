open Syntaxe
open Functions

(***************************************)

(*Returns a Num's value*)
let get_val e =
    match e with
    | Num(i) -> i
    | _ -> failwith "erreur get_val : appel sur autre que Const"

(*Applies the operator on e1 and e2*)
let apply_Op (o) (e1) (e2) =
    match o with
    | Add -> e1 + e2
    | Sub -> e1 - e2
    | Mul -> e1 * e2
    | Div -> e1 / e2
    | Mod -> e1 mod e2

(*Applies the comparator on e1 and e2*)
let apply_comp (comp) (e1) (e2) =
    match comp with
    | Eq -> e1 = e2
    | Ne -> e1 <> e2
    | Lt -> e1 < e2
    | Le -> e1 <= e2
    | Gt -> e1 > e2
    | Ge -> e1 >= e2

(*Returns true if e is a Num*)
let is_Const (e) =
    match e with
    | Num(i) -> true
    | _ -> false

(*Simplifies an expression*)
let simpl_exp (e) =
    match e with
    | Num(i) -> Num(i)
    | Var(n) -> Var(n)
    | Op(o,e1,e2) ->
        if is_Const e1 && is_Const e2 then
            Num(apply_Op o (get_val e1) (get_val e2))
        else if is_Const e1 && (get_val e1) = 0 then
            match o with
            | Add -> e2
            | Sub -> Op(o,e1,e2)
            | Mul -> Num(0)
            | Div -> Num(0)
            | Mod -> Num(0)
        else if is_Const e1 && (get_val e1) = 1 then
            match o with
            | Mul -> e2
            | _ -> Op(o,e1,e2)
        else if is_Const e2 && (get_val e2) = 0 then
            match o with
            | Add -> e1
            | Sub -> e1
            | Mul -> Num(0)
            | Div -> Op(o,e1,e2)
            | Mod -> Op(o,e1,e2)
        else if is_Const e2 && (get_val e2) = 1 then
            match o with
            | Mul -> e1
            | _ -> Op(o,e1,e2)
        else
            Op(o,e1,e2)

(*Simplifies a condition*)
let simpl_cond (c)=
    match c with
    | (e1, com, e2) -> ( (simpl_exp e1),com , (simpl_exp e2) )

(*Simplifies a Num*)
let rec simpl_const (p) (pos) (p2)=
    if (pos <= (max_pos p 0)) then
        (let inst = search_block pos p in
        match inst with
        | Set(n,e) -> 
            simpl_const p (pos+1) ( (pos, Set(n, (simpl_exp e)))::p2)
        | Read(n) -> 
            simpl_const p (pos+1) ( (pos, Read(n))::p2 )
        | Print(e) -> 
            simpl_const p (pos+1) ( (pos, Print(simpl_exp e))::p2 )
        | If(c,b1,b2) ->
            let nb1 = simpl_const b1 (pos+1) [] in
            let nb2 = simpl_const b2 (max_pos nb1 0 + 1) [] in
            let e = If( (simpl_cond c), nb1, nb2) in
            if (nb2 = []) then
                simpl_const p (max_pos nb1 0 +1) ( (pos, e)::p2 )
            else
                simpl_const p (max_pos nb2 0 +1) ( (pos, e)::p2 )
        | While(c,b) ->
            let nb = simpl_const b (pos+1) [] in
            let e = While( (simpl_cond c), nb) in
            simpl_const p (max_pos nb 0 +1) ( (pos, e)::p2 )
        )
    else
        p2

(*Returns true if cond is composed of 2 Num*)
let cond_with_const (cond) =
    match cond with
    | (e1, com, e2) -> 
        if is_Const e1 && is_Const e2 then
            true
        else false

let cond_is_true (cond)=
match cond with
| (e1, com, e2) -> 
    apply_comp com (get_val e1) (get_val e2)

(*Simplifies a block or a program*)
let rec simpl_block (p) (pos) (pos_instr) (p2) =
    if (pos <= max_pos p 0) then
        let inst = search_block pos p in
        match inst with
        | Set(n,e) -> 
            simpl_block p (pos+1) (pos_instr +1) ( (pos_instr,Set(n,e))::p2 )
        | Read(n) -> 
            simpl_block p (pos+1) (pos_instr +1) ( (pos_instr,Read(n))::p2 )
        | Print(e) -> 
            simpl_block p (pos+1) (pos_instr +1) ( (pos_instr,Print(e))::p2 )
        | If(c,b1,b2) ->
            if cond_with_const c then 
                if cond_is_true c then
                    let p3 = simpl_block b1 (pos+1) (pos_instr) [] in
                    if (b2 = []) then
                        simpl_block p (max_pos b1 0 +1) (max_pos p3 0 +1) (List.append p3 p2)
                    else
                        simpl_block p (max_pos b2 0 +1) (max_pos p3 0 +1) (List.append p3 p2)
                else
                    if b2 = [] then
                        simpl_block p (max_pos b1 0 +1) (pos_instr) p2
                    else
                        let p3 = simpl_block b2 (max_pos b1 0 +1) (pos_instr) [] in
                        simpl_block p (max_pos b2 0 +1) (max_pos p3 0 +1) (List.append p3 p2)
            else
                let p3 = simpl_block b1 (pos+1) (pos_instr+1) [] in
                let p4 = simpl_block b2 (max_pos b1 0 +1) (max_pos p3 0 +1) [] in
                let e = If(c,p3,p4) in
                if (b2 = []) then
                    simpl_block p (max_pos b1 0+1) (max_pos p3 0 +1) ((pos_instr,e)::p2)
                else
                    simpl_block p (max_pos b2 0+1) (max_pos p4 0 +1) ((pos_instr,e)::p2)
        | While(c,b) ->
            if cond_with_const c then
                if cond_is_true c then
                    let nb = simpl_block b (pos+1) (pos_instr + 1) [] in 
                    simpl_block p (max_pos b 0 +1) (max_pos nb 0 +1) ( (pos_instr,While(c,nb))::p2 )
                else
                    simpl_block p (max_pos b 0 +1) pos_instr p2
            else
                let nb = simpl_block b (pos +1) (pos_instr+1) [] in
                simpl_block p (max_pos b 0 +1) (max_pos nb 0 +1) ( (pos_instr, While(c,nb))::p2 )
            
    else 
        p2

(*Main function*)
let simpl_polish (p) =
    let p2 = simpl_const p 0 [] in
    simpl_block p2 0 0 []
    