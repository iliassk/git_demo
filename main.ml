open Syntax;;


type ty = TyBool | TyNat | TyNone;;


let rec term_to_tex t =
  match t with 
  | TmTrue -> "\\AxiomC{}\n\\UnaryInfC{true}\n";
  | TmFalse ->"\\AxiomC{}\n\\UnaryInfC{false}\n";
  | TmZero -> "\\AxiomC{}\n\\UnaryInfC{0}\n";
  | TmSucc u -> (term_to_tex u) ^ "\\UnaryInfC{" ^ (term_to_string t) ^ "}";
  | TmIsZ u -> (term_to_tex u) ^ "\\UnaryInfC{" ^ (term_to_string t) ^ "}";
  | TmIf (i,u,e) -> (term_to_tex i) ^ (term_to_tex u) ^ (term_to_tex e) ^
		      "\\TrinaryInfC{" ^ (term_to_string t) ^ "}";;

(* Exercise 3 : Basic types *)
(* 3 - An expression is well-typed if all its sub-expressions are well-typed as well, and correspond to an evaluation rule. *)


(* 4 - An algorithm that infers a typed derivation tree for a given expression. *)
let rec term_to_type t =
  match t  with 
  | TmTrue -> ("\\AxiomC{}\n\\UnaryInfC{true : Bool}\n", TyBool);
  | TmFalse ->("\\AxiomC{}\n\\UnaryInfC{false : Bool}\n", TyBool);
  | TmZero -> ("\\AxiomC{}\n\\UnaryInfC{0 : Nat}\n", TyNat);
  | TmSucc u -> ((fst (term_to_type u)) ^ "\\UnaryInfC{" ^ (term_to_string t) ^ " : Nat}", TyNat);
  | TmIsZ u -> ((fst (term_to_type u)) ^ "\\UnaryInfC{" ^ (term_to_string t) ^ " : Bool}", TyBool);
  | TmIf (i,u,e) -> let (ue, uty) = (term_to_type u) in
		    ((fst (term_to_type i)) ^ (ue) ^ (fst (term_to_type e)) ^ "\\TrinaryInfC{" ^ (term_to_string t) ^ " : " ^ 
			(if (uty == TyBool) then "Bool" else "Nat") ^ " }", uty);;


let term_to_deriv t =
 "\\begin{prooftree}\n" ^ (fst (term_to_type t)) ^ "\\end{prooftree}\n";;


let main = 
  print_string (term_to_string (Parser.parse_term 
				  "if (iszero (succ (succ 0))) then true else false"
	       ));
  print_string (term_to_deriv (Parser.parse_term 
				  "if (iszero (succ (succ 0))) then true else false"
	       ));

;;
