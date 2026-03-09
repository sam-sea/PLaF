(* Names: Samantha Cardo *)

open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds
    
(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val ea_result =
  fun e ->
  match e with
  | Int(n) ->
    return (NumVal n)
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1+n2))
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1-n2))
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1*n2))
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2=0
    then error "Division by zero"
    else return (NumVal (n1/n2))
  | Let(id,def,body) ->
    eval_expr def >>= 
    extend_env id >>+
    eval_expr body 
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n = 0))
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"

  (* ── 3.1  Lists ──────────────────────────────────────────────────── *)

  (* emptylist() → ListVal [] *)
  | EmptyList(_t) ->
    return (ListVal [])

  (* cons(e1, e2): e2 must be a ListVal; prepend the value of e1 *)
  | Cons(e1, e2) ->
    eval_expr e1 >>= fun v1 ->
    eval_expr e2 >>= fun v2 ->
    list_of_listVal v2 >>= fun lst ->
    return (ListVal (v1 :: lst))

  (* hd(e): return first element; error on empty list *)
  | Hd(e) ->
    eval_expr e >>=
    list_of_listVal >>= fun lst ->
    (match lst with
     | [] -> error "hd: empty list!"
     | h :: _ -> return h)

  (* tl(e): return tail; error on empty list *)
  | Tl(e) ->
    eval_expr e >>=
    list_of_listVal >>= fun lst ->
    (match lst with
     | [] -> error "tl: empty list!"
     | _ :: t -> return (ListVal t))

  (* empty?(e): true iff the list is empty; error if not a list *)
  | IsEmpty(e) ->
    eval_expr e >>=
    list_of_listVal >>= fun lst ->
    return (BoolVal (lst = []))

  (* ── 3.2  Tuples ─────────────────────────────────────────────────── *)

  (* <e1,...,en> → TupleVal [v1;...;vn] *)
  | Tuple(es) ->
    eval_exprs es >>= fun vs ->
    return (TupleVal vs)

  (* let <id1,...,idn> = e1 in e2:
     evaluate e1, unpack TupleVal, bind ids, evaluate e2 *)
  | Untuple(ids, e1, e2) ->
    eval_expr e1 >>= fun v ->
    list_of_tupleVal v >>= fun evs ->
    extend_env_list ids evs >>+
    eval_expr e2

  | _ -> failwith "Not implemented yet!"

(** [eval_exprs es] evaluates a list of expressions left-to-right *)
and eval_exprs : expr list -> (exp_val list) ea_result =
  fun es ->
  match es with
  | [] -> return []
  | h :: t ->
    eval_expr h >>= fun v ->
    eval_exprs t >>= fun vs ->
    return (v :: vs)

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c
