open Read
open Print
open Eval
open Simpl
open Sign
open Vars


let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "Entrez une ligne de commande valide.\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> 
    let pro = read_polish file in 
    print_polish pro
  | [|_;"-eval";file|] -> 
    let pro = read_polish file in 
    eval_polish pro
  | [|_;"-sign";file|] -> 
    let pro = read_polish file in 
    sign_polish pro
  | [|_;"-simpl";file|] -> Print.print_polish (Simpl.simpl_polish (Read.read_polish file))
  | [|_;"-vars";file|] -> 
    let pro = read_polish file in 
    vars_polish pro
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
