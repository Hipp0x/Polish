(* Searches and returns the block at position pos or fails if it doesn't exist *)
let rec search_block (pos) (b) =
  match b with
  | [] -> failwith "NO"

  | (x,y)::l -> if x = pos then
                  y
                else
                  search_block pos l
;;

(* Returns the highest position of an instruction in a block *)
let rec max_pos (b) (max)= 
  match b with
  | [] -> max

  | (x,y)::l -> if (x > max) then
                  max_pos l x
                else
                  max_pos l max
;;

(* Returns the smallest position of an instruction in a block *)
let rec min_instr (b) (min)= 
  match b with
  | [] -> min

  | (x,y)::l -> if (x <= min) then
                  min_instr l x
                else
                  min_instr l min
;;

(*Returns the next instruction's position*)
let rec next_pos (pos) (current) (p) =
  match p with
  | [] -> (current)

  | (x,y)::l -> if current < x then
                  next_pos pos current l
                else
                    if current > x && x > pos then
                      next_pos pos x l
                    else
                      next_pos pos current l
;;  