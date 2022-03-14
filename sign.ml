open Syntaxe
open Functions

let is_var e =
  match e with
  | Var(_) -> true
  | _ -> false

let is_num e =
  match e with
  | Num(_) -> true
  | _ -> false

let is_expr expr =
  match expr with
  | Op(_) -> true
  | _ -> false

let get_val e =
  match e with
  | Var(x) -> x
  | _ -> failwith " ce n'est pas une Var(x)"

let get_num e =
  match e with
  | Num(x) -> x
  | _ -> failwith " ce n'est pas un Num(x)"
    
  
(*Returns true if exp exist in the environnement*)
let rec is_in_list (liste) (exp) =
  match liste with
  | [] -> false
  | (e,_)::l ->
    if (e = exp) then
      true
    else
    is_in_list l exp

let rec union_liste l1 l2 =
  if List.length l1 = 0 then
    l2
  else 
    (
  let e = List.hd l1 in
  if (List.exists (fun x -> x = e) l2) then
    union_liste (List.tl l1) l2
  else
    union_liste (List.tl l1) (List.cons e l2)
    )
;;

(*Fuses 2 lists with the add operator*)
let join_add l1 l2=
  if ( List.exists (fun x -> x = Pos) l1 && List.exists (fun x -> x = Neg) l2 ) ||
       (List.exists (fun x -> x = Neg) l1 && List.exists (fun x -> x = Pos) l2 ) then
      Pos::Zero::Neg::[]

  else if (l1 = Pos::[] || l2 = Pos::[]) then
    Pos::[]

  else if (l1 = Neg::[] || l1 = Neg::[]) then
    Neg::[]

  else if (l1 = Zero::[] || l2 = Zero::[]) then
    Zero::[]

  else if ( (List.exists (fun x -> x = Zero) l1 && List.exists (fun x -> x = Pos) l1 && not (List.exists (fun x -> x = Neg) l1) ) ||
    (List.exists (fun x -> x = Zero) l2 && List.exists (fun x -> x = Pos) l2 && not (List.exists (fun x -> x = Neg) l2) ) ) then
    Pos::Zero::[]

  else if ( (List.exists (fun x -> x = Zero) l1 && List.exists (fun x -> x = Neg) l1 && not (List.exists (fun x -> x = Pos) l1) ) ||
    (List.exists (fun x -> x = Zero) l2 && List.exists (fun x -> x = Neg) l2 && not (List.exists (fun x -> x = Pos) l2) ) ) then
    Zero::Neg::[]

  else if ( (List.exists (fun x -> x = Zero) l1 && List.exists (fun x -> x = Pos) l1 && List.exists (fun x -> x = Neg) l1 ) ||
    (List.exists (fun x -> x = Zero) l2 && List.exists (fun x -> x = Pos) l2 && List.exists (fun x -> x = Neg) l2)  ) then
    Pos::Zero::Neg::[]

  else
    failwith "Erreur dans la jonction des listes de signs dans l'addition"

 

(*Fuses 2 lists with the sub operator*)
let join_sub l1 l2 =
 if List.exists (fun x -> x = Pos) l1 && List.exists (fun x -> x = Pos) l2 then
    Pos::Zero::Neg::[]

 else if (l1 = Pos::Zero::Neg::[] || l2 = Pos::Zero::Neg::[]) then
    Pos::Zero::Neg::[]

 else if (l1 = Neg::[]) then
      if (List.exists (fun x -> x = Neg) l2) then
        Pos::Zero::Neg::[]
      else
        Neg::[]

  else if (l1 = Pos::[]) then
    Pos::[]

  else if (l2 = Pos::Zero::[]) then
    Zero::Neg::[]

  else if (l2 = Pos::[]) then
    Neg::[]

  else if (l1 = Zero::[]) then
      if (l2 = Neg::[]) then 
        Pos::[]
      else if (l2 = Zero::[]) then
        Zero::[]
      else 
        Pos::Zero::[]
    
  else if l1 = Zero::Neg::[] then
      if (l2 = Zero::[]) then
        Zero::Neg::[]
      else
        Pos::Zero::Neg::[]
    
  else if (l1 = Pos::Zero::[]) then
      if l2 = Neg::[] then
        Pos::[] 
      else
        Pos::Zero::[]
    
  else
    failwith "Erreur dans la jonction des listes de signs dans la soustraction"
;;

(*Fuses 2 lists with the mult operator*)
let join_mul l1 l2 =
  if List.exists (fun x -> x = Zero) l2 || List.exists (fun x -> x = Zero) l1 then
   Zero::[]
  else if l1 = Pos::[] && l2 = Pos::[]  then
    Pos::[]
  else if l1 = Neg::[] && l2 = Neg::[] then
    Pos::[]
  else if (l1 = Pos::[] && l2 = Neg::[]) || (l2 = Pos::[] && l1 = Neg::[]) then
    Neg::[]
  else
    failwith "Erreur dans la jonction des listes de signs dans la multiplication"

(*Fuses 2 lists with the div operator*)
let join_div l1 l2 =
  if List.exists (fun x -> x = Zero) l2  then (
    if List.exists (fun x -> x = Zero) l1  then
      Zero::Error::[]
    else if (l1 = Pos::[] && l2 = Pos::[]) || (l1 = Neg::[] && l2 = Neg::[])  then
      Pos::Error::[]
    else if (l1 = Neg::[] && l2 = Pos::[]) || (l1 = Pos::[] && l2 = Neg::[]) then
      Neg::Error::[]
    else
      Pos::Neg::Error::[] )
  else
      if List.exists (fun x -> x = Zero) l1  then
        Zero::[]
      else if (l1 = Pos::[] && l2 = Pos::[]) || (l1 = Neg::[] && l2 = Neg::[])  then
        Pos::[]
      else if (l1 = Neg::[] && l2 = Pos::[]) || (l1 = Pos::[] && l2 = Neg::[]) then
        Neg::[]
      else
        Pos::Neg::[]
    

(*Fuses 2 lists with the mod operator*)
let join_mod l1 l2 =
  if List.exists (fun x -> x = Zero) l2  then (
    if List.exists (fun x -> x = Zero) l1  then
      Zero::Error::[]
    else if (l1 = Pos::[] || l1 = Neg::[]) && l2 = Pos::[]  then
      Pos::Error::[]
    else if (l1 = Pos::[] || l1 = Neg::[]) && l2 = Neg::[] then
      Neg::Error::[]
    else
      Pos::Neg::Error::[])
  else (
    if List.exists (fun x -> x = Zero) l1  then
      Zero::[]
    else if (l1 = Pos::[] || l1 = Neg::[]) && l2 = Pos::[]  then
      Pos::[]
    else if (l1 = Pos::[] || l1 = Neg::[]) && l2 = Neg::[] then
      Neg::[]
    else
      Pos::Neg::[])

(*Defines an expression's signs*)
let rec sign_expr (exp) (liste) =
  match exp with
  | Num(i) -> 
    if i = 0 then
      [Zero]
    else if i < 0 then
      [Neg]
    else
      [Pos]
  | Var(n) ->
    (try SignTable.find n liste with Not_found -> (Pos::Neg::Zero::[]))
  | Op(o,e1,e2) ->
    let l1 = sign_expr e1 liste in
    let l2 = sign_expr e2 liste in
    match o with
    | Add -> join_add l1 l2
    | Sub -> join_sub l1 l2
    | Mul -> join_mul l1 l2
    | Div -> join_div l1 l2
    | Mod -> join_mod l1 l2

let is_possible_Eq (l1) (l2) =
  l1 = l2

let is_possible_Ne l1 l2 = 
  l1 != l2

let is_possible_Lt l1 l2 =
  if List.exists (fun x -> x = Neg) l1 then
    true
  else if List.exists (fun x -> x = Zero) l1 then
    if (List.exists (fun x -> x = Zero) l2 || List.exists (fun x -> x = Neg) l2) then
      false
    else  
      true
  else if (List.exists (fun x -> x = Pos) l1) then
    false
  else
    failwith "erreur is possible < (Lt)"   

let is_possible_Le l1 l2 =
  if List.exists (fun x -> x = Neg) l1 then
    true
  else if List.exists (fun x -> x = Zero) l1 then
    if List.exists (fun x -> x = Pos) l2 || List.exists (fun x -> x = Zero) l2 then
      true
    else
      false
  else if List.exists (fun x -> x = Pos) l1 then
    if (List.exists (fun x -> x = Zero) l2 || List.exists (fun x -> x = Neg) l2) then
      false
    else  
      true
  else
    failwith "erreur is possible <= (Le)"   

let is_possible_Gt l1 l2 =
  if List.exists (fun x -> x = Pos) l1 then
    true
  else if List.exists (fun x -> x = Zero) l1 then
    if List.exists (fun x -> x = Pos) l2 || List.exists (fun x -> x = Zero) l2 then
      false
    else 
      true
  else if List.exists (fun x -> x = Neg) l1 then
    false
  else
    failwith "erreur is possible > (Gt)"    

let is_possible_Ge l1 l2 =
  if List.exists (fun x -> x = Pos) l1 then
    true
  else if List.exists (fun x -> x = Zero) l1 then
    if List.exists (fun x -> x = Pos) l2 then
      false
    else
      true
  else if List.exists (fun x -> x = Neg) l1 then
    if l2 != Neg::[] then
      false
    else
      true
  else
    failwith "erreur is possible >= (Ge)"    
    
(*Establishes if a cond is possible considering the environnement*)
let cond_is_possible c e1 e2 liste =
  if (is_num(e1) && is_num(e2)) then
    (match c with
    | Eq -> if get_num(e1) == get_num(e2) then true else false
    | Ne -> if get_num(e1) != get_num(e2) then true else false
    | Lt -> if get_num(e1) < get_num(e2) then true else false
    | Le -> if get_num(e1) <= get_num(e2) then true else false
    | Gt -> if get_num(e1) > get_num(e2) then true else false 
    | Ge -> if get_num(e1) >= get_num(e2) then true else false )
  else (
  let l1 = sign_expr e1 liste in
  let l2 = sign_expr e2 liste in 
  match c with
  | Eq -> is_possible_Eq l1 l2 
  | Ne -> is_possible_Ne l1 l2 
  | Lt -> is_possible_Lt l1 l2 
  | Le -> is_possible_Le l1 l2 
  | Gt -> is_possible_Gt l1 l2 
  | Ge -> is_possible_Ge l1 l2 )

(*Defines a cond's signs and potentally updates them.
  The condition is established to be possible*)
let sign_cond (c) (e1) (e2) (liste) =
  let l1 = sign_expr e1 liste in
  let l2 = sign_expr e2 liste in 
  match c with
  | Eq -> l2

  | Ne -> 
      let rec funct l1 l2 =
        if l2 = [] then l1 
        else (
        if (List.exists (fun x -> x = List.hd l2) l1) then
          funct (List.filter (fun x -> x != List.hd l2) l1) (List.tl l2)
        else
          funct (l1) (List.tl l2) )
        in
        funct l1 l2 
    
  | Lt -> 
        if List.exists (fun x -> x = Pos) l2 then
          l1
        else if List.exists (fun x -> x = Zero) l2 then
          Neg::[]
        else
          Neg::[]
  
  | Le -> 
    if List.exists (fun x -> x = Pos) l2 then
      l1
    else if List.exists (fun x -> x = Zero) l2 then
      l1
    else
      Neg::[] 
  
  | Gt -> 
      if List.exists (fun x -> x = Neg) l2 then
        l1
      else if List.exists (fun x -> x = Zero) l2 then
        Pos::[]
      else
        Pos::[]
    
  | Ge -> 
      if List.exists (fun x -> x = Neg) l2 then
        l1
      else if List.exists (fun x -> x = Zero) l2 then
        l1
      else
        Pos::[]
    

(*Returns the reverse comp*)
let cond_inverse (c)=
  match c with 
  | Eq -> Ne
  | Ne -> Eq
  | Lt -> Ge
  | Le -> Gt
  | Gt -> Le
  | Ge -> Lt

  (*Returns the opposite comp*)
  let opposite_comp c =
    match c with 
    | Lt -> Gt
    | Gt -> Lt
    | Le -> Ge
    | Ge -> Le
    | _ -> c
  ;;

  (*Returns true if we have to switch expr in a condition*)
  let doSwitch e1 e2 c =
    if not(is_var e1) then
      if  is_var e2 then
        true
      else
        false
    else
      false
  ;;

let rec union_error l1 l2 =
  if (List.length l1 = 0) then
    l2
  else if (List.length l2 = 0) then
    l1 
  else if (List.exists (fun x -> x = List.hd l1) l2) then
      union_error (List.tl l1) (l2)
  else
      union_error (List.tl l1) (List.cons (List.hd l1) l2)

let rec union_sign (l1) (l2) =
  if SignTable.is_empty l1 then 
    l2
  else
    let va = SignTable.choose l1 in
    match va with
    | (k,d) ->
      if SignTable.mem k l2 then 
          let e = SignTable.find k l2 in
          let l = union_liste d e in 
          union_sign (SignTable.remove k l1) (SignTable.add k l l2)
      else
        union_sign (SignTable.remove k l1) (SignTable.add k d l2)


let rec check_vars_is_init (expr) (signs) =         
  match expr with
  |Num(x) -> true
  |Var(name) -> 
    if SignTable.exists (fun x y -> x = name) signs then
      true
    else 
      false
  |Op(op,expr1,expr2) ->  check_vars_is_init expr1 signs && check_vars_is_init expr2 signs

(*: ([SignTable]::[int list])*)
let rec prop_sign (p) (pos) (signs) (errors) =
  if (pos <= max_pos p 0) then
    (
      let inst = search_block pos p in
      match inst with

      | Set(n,e) ->
        let l = sign_expr e signs in
        prop_sign p (pos+1) (SignTable.add n l signs) (if List.exists (fun x -> x = Error) l then pos::errors else errors)

      | Read(n) ->
        let l = sign_expr (Var(n)) signs in
        prop_sign p (pos+1) (SignTable.add n l signs) errors

      | Print(e) ->
        if check_vars_is_init e signs then
          prop_sign p (pos+1) signs errors
        else 
          failwith "Variable non initialisée"

      | If((e1,c,e2),b1,b2) ->
        if ( (check_vars_is_init e1 signs) = false || (check_vars_is_init e2 signs) = false ) then
            failwith "Variable non initialisée"
        else (
            let flag = doSwitch e1 e2 c in
            let tmp = e1 in
            let e1 = (if flag then
                        e2
                      else
                        e1)
            in

            let e2 = (if flag then
                        tmp
                      else
                        e2)
            in

            let c = (if flag then
                      opposite_comp c
                    else
                      c) 
            in
            let lc1 = cond_is_possible c e1 e2 signs in
            let cond1 = (if lc1 then sign_cond c e1 e2 signs else []) in
            let cc = cond_inverse c in 
            let lc2 = cond_is_possible cc e1 e2 signs in
            let cond2 = (if lc2 then sign_cond cc e1 e2 signs else []) in
            let signs1 = (
                if (is_var e1) then(
                    let value = get_val e1 in
                    SignTable.add value cond1 signs)
                else
                  signs) in
            let signs2 = (
                if (is_var e1) then(
                  let value = get_val e1 in
                  SignTable.add value cond2 signs)
                else
                  signs ) in 
            let errors = (if is_expr e1 && is_expr e2 then
                            (let a = (if List.exists (fun x -> x = Error) (sign_expr e1 signs) then pos::errors else errors) in
                            let b = (if List.exists (fun x -> x = Error) (sign_expr e2 signs) then pos::errors else errors) in
                            union_error a b)
                          else if is_expr e1 then
                            if List.exists (fun x -> x = Error) (sign_expr e1 signs) then pos::errors else errors
                          else if is_expr e2 then
                            if List.exists (fun x -> x = Error) (sign_expr e2 signs) then pos::errors else errors
                          else
                            errors)
            in
            if b2 = [] then (
                if lc1 = false then
                      prop_sign p (max_pos b1 0 +1) signs errors
                else (
                    let liste_fin = prop_sign b1 (pos +1) signs1 (if List.exists (fun x -> x = Error) cond1 then pos::errors else errors) in
                    match liste_fin with
                    | (x,y) -> prop_sign p (max_pos b1 0 +1) (union_sign x signs) (union_error y errors))
              )
            else (
                if lc1 = false then (
                  if lc2 = false then
                    prop_sign p (max_pos b2 0 +1) signs errors
                  else (
                      let liste_fin = prop_sign b2 (max_pos b1 0 +1) signs2 (if List.exists (fun x -> x = Error) cond2 then pos::errors else errors) in
                      match liste_fin with 
                      | (x,y) -> prop_sign p (max_pos b2 0 +1) (union_sign x signs) (union_error y errors)) 
                  )
                else (
                    let liste_fin = prop_sign b1 (pos +1) signs1 (if List.exists (fun x -> x = Error) cond1 then pos::errors else errors) in
                      match liste_fin with
                      | (x,y) -> (
                          if lc2 = false then 
                            prop_sign p (max_pos b2 0 +1) (union_sign x signs) (union_error y errors)
                          else (
                            let liste_fin2 = prop_sign b2 (max_pos b1 0 +1) signs2 (if List.exists (fun x -> x = Error) cond2 then pos::errors else errors) in
                            match liste_fin2 with
                            | (xx,yy) -> prop_sign p (max_pos b2 0 +1) (union_sign x xx) (union_error y yy)))
                  )
              )
          )
      | While((e1,c,e2),b) ->
        if ( (check_vars_is_init e1 signs) == false || (check_vars_is_init e2 signs) == false ) then
          failwith "Variable non initialisée"
        else (
              let rec parcours_while env1 =
              match env1 with
              | (x,y) -> ( 
                    if cond_is_possible c e1 e2 x then (
                        let signs1 = (
                          if (is_var e1) then(
                            let value = get_val e1 in
                            SignTable.add value (sign_cond c e1 e2 x) signs)
                          else
                            x ) in
                        let envx = prop_sign b (pos+1) signs1 y in
                        match envx with
                        | (xx,yy) -> (
                          if (x = xx) then
                            env1
                          else 
                            parcours_while envx ))
                    else
                      env1)
              in
              let env_fin = parcours_while (signs,errors) in
              match env_fin with
              | (s,e) ->(
                  let cc = cond_inverse c in
                  let cond_inv = sign_cond cc e1 e2 s in
                  let signs1 = (
                  if (is_var e1) then(
                      let value = get_val e1 in
                      SignTable.add value cond_inv s)
                  else
                    s) in
                  prop_sign p (max_pos b 0 +1) (union_sign s signs) (union_error e errors)))
    )
  else
    (signs,errors)


(*Prints a sign list*)
let print_list_sign (liste) =
  if List.exists (fun x -> x = Pos) liste then
    print_string "+ ";
  if List.exists (fun x -> x = Zero) liste then
    print_string "0 ";
  if List.exists (fun x -> x = Neg) liste then
    print_string "- ";
  if List.exists (fun x -> x = Error) liste then
    print_string "! "

(*Prints the possible sign list for each variable*)
let print_list (liste) =
  SignTable.iter (fun x y -> 
    (print_string (x^" ");
    print_list_sign y;
    print_string "\n") ) liste


(*Main function*)
let sign_polish (p) : unit = 
  let signs = SignTable.empty in
  let liste = prop_sign p 0 signs [] in
  match liste with
  | (x,y) ->
    print_list x;
    if y = [] then print_string "safe\n"
    else  
    List.iter (fun z -> 
      (print_int z;
        print_string " : Division by 0\n") ) y