datatype exp = Const of int
	     | Negate of exp
	     | Add of exp * exp
	     | Multiply of exp * exp

(* old *)
(* exp -> int *)
				     
fun eval_exp_old e =
  case e of
      Const i => i
    | Negate e2 => ~ (eval_exp_old e2)
    | Add(e1, e2) => (eval_exp_old e1) + (eval_exp_old e2)
    | Multiply(e1, e2) => (eval_exp_old e1) * (eval_exp_old e2)

exception Error of string

(* new *)
(* exp -> exp *)
		       
fun eval_exp_new e =
  let fun get_int e =
	case e of
	    Const i => i
	  | _ => raise (Error "expected Const result")
  in
      case e of
	  Const _ => e
	| Negate e2 => Const (~ (get_int (eval_exp_new e2)))
	| Add(e1, e2) => Const ((get_int (eval_exp_new e1)) +
				(get_int (eval_exp_new e2)))
	| Multiply(e1, e2) => Const ((get_int (eval_exp_new e1)) *
				     (get_int (eval_exp_new e2)))
  end

val test_exp = Multiply (Negate (Add (Const 2, Const 2)), Const 7)
val old_test = eval_exp_old test_exp = ~28
val new_test = eval_exp_new test_exp = Const ~28

			    
      
  
			   
