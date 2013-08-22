(* global types and utility functions *)
(* module Lb = Label_only *)
    (* circular with Lb *)
    
let ramification_entailments = ref 0
let noninter_entailments = ref 0
let total_entailments = ref 0

type aliasing_scenario = 
  | Not_Aliased
  | May_Aliased
  | Must_Aliased
  | Partial_Aliased

type ('a,'b) twoAns = 
  | FstAns of 'a
  | SndAns of 'b

type ident = string
type constant_flow = string

exception Illegal_Prover_Format of string
exception SA_HP_TUPLED

let reverify_flag = ref false
let reverify_all_flag = ref false
let ineq_opt_flag = ref false

let illegal_format s = raise (Illegal_Prover_Format s)

type lemma_kind = LEM_TEST | LEM_TEST_NEW | LEM | LEM_UNSAFE | LEM_SAFE | LEM_INFER

(* type nflow = (int*int)(\*numeric representation of flow*\) *)
type flags = 
	  Flag_str of string
	| Flag_int of int
	| Flag_float of float
	
type bformula_label = int
and ho_branch_label = string
(*and branch_label = spec_label	(*formula branches*)*)

type formula_label = (int*string)

and control_path_id_strict = formula_label

and control_path_id = control_path_id_strict  option
    (*identifier for if, catch, call*)

let eq_control_path_id ((p1,_):formula_label) ((p2,_):formula_label) = p1==p2

let empty_label = (0,"")
let app_e_l c = (empty_label, c)
let combine_lbl (i1,s1)(i2,s2) = match s1 with 
  | "" -> (match s2 with 
            | "" -> (i1,s1)
            | _ -> (i2,s2))
  | _ -> (i1,s1)


type path_label = int (*which path at the current point has been taken 0 -> then branch or not catch or first spec, 1-> else or catch taken or snd spec...*)

type path_trace = (control_path_id_strict * path_label) list

and loc =  {
    start_pos : Lexing.position (* might be expanded to contain more information *);
    mid_pos : Lexing.position;
    end_pos : Lexing.position;
  }

and primed =
  | Primed
  | Unprimed

and heap_ann = Lend | Imm | Mutable | Accs

and vp_ann =  VP_Zero | VP_Full | VP_Value (* | VP_Ref *)

and term_ann = 
  | Term    (* definitely terminates *)
  | Loop    (* definitely loops *)
  | MayLoop (* don't know *)
  | Fail of term_fail    (* failed because of invalid trans *)

and term_fail =
  | TermErr_May
  | TermErr_Must

and rel = REq | RNeq | RGt | RGte | RLt | RLte | RSubAnn

type hp_arg_kind=
  | I
  | NI

let print_arg_kind i= match i with
  | I -> ""
  | NI -> "#"

(* and prim_type =  *)
(*   | TVar of int *)
(*   | Bool *)
(*   | Float *)
(*   | Int *)
(*   | Void *)
(*   | BagT of prim_type *)
(*   | List *)

(* TODO : move typ here in future *)
type typ =
  | UNK 
  | TVar of int
  | AnnT
  | Bool
  | Float
  | Int
  | INFInt
  | NUM
  | Void
  | List of typ
  | BagT of typ
  (* | Prim of prim_type *)
  | Named of ident (* named type, could be enumerated or object *)
          (* Named "R" *)
  | Array of (typ * int) (* base type and dimension *)
  | RelT of (typ list) (* relation type *)
  | HpT (* heap predicate relation type *)
  | Tree_sh
  (* | FuncT (\* function type *\) *)
  | Pointer of typ (* base type and dimension *)

let is_program_pointer (name:ident) = 
  let slen = (String.length name) in
  try  
      let n = (String.rindex name '_') in
      (* let _ = print_endline ((string_of_int n)) in *)
      let l = (slen-(n+1)) in
      if (l==0) then (false,name)
      else 
        let str = String.sub name (n+1) (slen-(n+1)) in
        if (str = "ptr") then
          let s = String.sub name 0 n in
          (true,s)
        else
          (false,name)
  with  _ -> (false,name)

let is_pointer_typ (t:typ) : bool =
  match t with
    | Pointer _ -> true
    | _ -> false

let convert_typ (t:typ) : typ =
  match t with
    | Pointer t1 -> 
        (match t1 with
          | Int -> Named "int_ptr"
          | Pointer t2 ->
              (match t2 with
                | Int -> Named "int_ptr_ptr"
                | _ -> t2 (*TO CHECK: need to generalize for float, bool, ...*)
              )
          | _ -> t1 (*TO CHECK: need to generalize for float, bool, ...*)
        )
    | _ -> t

let revert_typ (t:typ) : typ =
  (match t with
    | Named t1 ->
        (match t1 with
          | "int_ptr" -> Int
          | "int_ptr_ptr" -> Named "int_ptr"
          | _ -> Named "Not_Support")
    | _ -> Named "Not_Support")

let name_of_typ (t:typ) : string =
  (match t with
    | Named t1 ->
        t1
    | _ -> 
        "Not_Support")

let is_pointer t=
 match t with
   | Named _ -> true
   | _ -> false

let barrierT = Named "barrier"

let convert_prim_to_obj (t:typ) : typ =
  (match t with
    | Int -> Named "int_ptr"
    | Named t1 ->
        (match t1 with
          | "int_ptr" -> Named "int_ptr_ptr"
          | _-> t (*TO CHECK: need to generalize for float, bool, ...*)
        )
    | _ -> t (*TO CHECK: need to generalize for float, bool, ...*)
  )

(*for heap predicate*)
let hp_default_prefix_name = "HP_"
let hppost_default_prefix_name = "GP_"
let dang_hp_default_prefix_name = "__DP"
(*
  Data types for code gen
*)

type mode = 
  | ModeIn
  | ModeOut
  


type perm_type =
  | NoPerm (*no permission at all*)
  | Frac (*fractional permissions*)
  | Count (*counting permissions*)
  | Dperm (*distinct fractional shares*)
  
let perm = ref NoPerm

let no_pos = 
	let no_pos1 = { Lexing.pos_fname = "";
				   Lexing.pos_lnum = 0;
				   Lexing.pos_bol = 0; 
				   Lexing.pos_cnum = 0 } in
	{start_pos = no_pos1; mid_pos = no_pos1; end_pos = no_pos1;}

let is_no_pos l = (l.start_pos.Lexing.pos_cnum == 0)

let is_float_type (t:typ) = match t with
  | Float -> true
  | _ -> false

let string_of_heap_ann a =
  match a with
    | Accs -> "@A"
    | Lend -> "@L"
    | Imm -> "@I"
    | Mutable -> "@M"

let int_of_heap_ann a =
  match a with
    | Accs -> 3
    | Lend -> 2
    | Imm -> 1
    | Mutable -> 0

let string_of_vp_ann a =  
  (match a with
    | VP_Zero -> "@zero"
    | VP_Full -> "@full"
    | VP_Value -> "@value"
    (* | VP_Ref-> "@p_ref" *)
  )

let string_of_term_ann a =
  match a with
    | Term -> "Term"
    | Loop -> "Loop"
    | MayLoop -> "MayLoop"
    | Fail f -> match f with
        | TermErr_May -> "TermErr_May"
        | TermErr_Must -> "TermErr_Must"

let string_of_loc (p : loc) = 
    Printf.sprintf "1 File \"%s\",Line:%d,Col:%d"
    p.start_pos.Lexing.pos_fname 
    p.start_pos.Lexing.pos_lnum
    (p.start_pos.Lexing.pos_cnum-p.start_pos.Lexing.pos_bol)
;;

let string_of_pos (p : Lexing.position) = 
    Printf.sprintf "(Line:%d,Col:%d)"
    p.Lexing.pos_lnum
	(p.Lexing.pos_cnum-p.Lexing.pos_bol)
;;

let string_of_pos_plain (p : Lexing.position) = 
    Printf.sprintf "%d_%d"
    p.Lexing.pos_lnum
    (p.Lexing.pos_cnum-p.Lexing.pos_bol)
;;

(* let string_of_pos (p : Lexing.position) = "("^string_of_int(p.Lexing.pos_lnum) ^","^string_of_int(p.Lexing.pos_cnum-p.Lexing.pos_bol) ^")" *)
(* ;; *)

(* An Hoa *)
let line_number_of_pos p = string_of_int (p.start_pos.Lexing.pos_lnum)

let string_of_full_loc (l : loc) = "{"^(string_of_pos l.start_pos)^","^(string_of_pos l.end_pos)^"}";;

let string_of_loc_by_char_num (l : loc) = 
  Printf.sprintf "(%d-%d)"
    l.start_pos.Lexing.pos_cnum
    l.end_pos.Lexing.pos_cnum

(* class prog_loc = *)
(*    object  *)
(*      val mutable lc = None *)
(*      method is_avail : bool = match lc with *)
(*        | None -> false *)
(*        | Some _ -> true *)
(*      method set (nl:loc) = lc <- Some nl *)
(*      method get :loc = match lc with *)
(*        | None -> no_pos *)
(*        | Some p -> p *)
(*      method reset = lc <- None *)
(*      method string_of : string = match lc with *)
(*        | None -> "None" *)
(*        | Some l -> (string_of_loc l) *)
(*      method string_of_pos : string = match lc with *)
(*        | None -> "None" *)
(*        | Some l -> (string_of_pos l.start_pos) *)
(*    end;; *)

(* Option for proof logging *)
let proof_logging = ref false
let proof_logging_txt = ref false
let log_proof_details = ref true
let proof_logging_time = ref 0.000
(* let sleek_src_files = ref ([]: string list) *)

(*sleek logging*)
let sleek_logging_txt = ref false
let dump_proof = ref false
let dump_sleek_proof = ref false

(*Proof logging facilities*)
class ['a] store (x_init:'a) (epr:'a->string) =
   object (self)
     val emp_val = x_init
     val mutable lc = None
     method is_avail : bool = match lc with
       | None -> false
       | Some _ -> true
     method set (nl:'a) = lc <- Some nl
     method get :'a = match lc with
       | None -> emp_val
       | Some p -> p
     method reset = lc <- None
     method get_rm :'a = match lc with
       | None -> emp_val
       | Some p -> (self#reset; p)
     method string_of : string = match lc with
       | None -> "Why None?"
       | Some l -> (epr l)
     method dump = print_endline ("\n store dump :"^(self#string_of))
   end;;

(* this will be set to true when we are in error explanation module *)
class failure_mode =
object
  inherit [bool] store false string_of_bool
end;;


class prog_loc =
object
  inherit [loc] store no_pos string_of_loc
     method string_of_pos : string = match lc with
       | None -> "None"
       | Some l -> (string_of_pos l.start_pos)
end;;


(*Some global vars for logging*)
let proving_loc  = new prog_loc
let post_pos = new prog_loc
let explain_mode = new failure_mode
let return_exp_pid = ref ([]: control_path_id list)	
let z3_proof_log_list = ref ([]: string list)
let z3_time = ref 0.0

let add_to_z3_proof_log_list (f: string) =
	z3_proof_log_list := !z3_proof_log_list @ [f]


let entail_pos = ref no_pos
let set_entail_pos p = entail_pos := p

(* let set_proving_loc p = proving_loc#set p *)
(*   (\* proving_loc := Some p *\) *)

(* let clear_proving_loc () = proving_loc#reset *)
(*   (\* proving_loc := None *\) *)

 let pr_lst s f xs = String.concat s (List.map f xs)

 let pr_list_brk open_b close_b f xs  = open_b ^(pr_lst ";" f xs)^close_b
 let pr_list f xs = pr_list_brk "[" "]" f xs
 let pr_list_angle f xs = pr_list_brk "<" ">" f xs
 let pr_list_round f xs = pr_list_brk "(" ")" f xs

(* pretty printing for types *)
let rec string_of_typ (x:typ) : string = match x with
   (* may be based on types used !! *)
  | UNK          -> "Unknown"
  | Bool          -> "boolean"
  | Float         -> "float"
  | Int           -> "int"
  | INFInt        -> "INFint"
  | Void          -> "void"
  | NUM          -> "NUM"
  | AnnT          -> "AnnT"
  | BagT t        -> "bag("^(string_of_typ t)^")"
  | TVar t        -> "TVar["^(string_of_int t)^"]"
  | List t        -> "list("^(string_of_typ t)^")"
  | Tree_sh		  -> "Tsh"
  | RelT a      -> "RelT("^(pr_list string_of_typ a)^")"
  | Pointer t        -> "Pointer{"^(string_of_typ t)^"}"
  | HpT        -> "HpT"
  | Named ot -> if ((String.compare ot "") ==0) then "null" else ot
  | Array (et, r) -> (* An Hoa *)
	let rec repeat k = if (k <= 0) then "" else "[]" ^ (repeat (k-1)) in
		(string_of_typ et) ^ (repeat r)
;;

let is_RelT x =
  match x with
    | RelT _ -> true
    | _ -> false
;;
let is_HpT x =
  match x with
    | HpT -> true
    | _ -> false
;;

(* aphanumeric name *)
let rec string_of_typ_alpha = function 
   (* may be based on types used !! *)
  | UNK          -> "Unknown"
  | Bool          -> "boolean"
  | Float         -> "float"
  | Int           -> "int"
  | INFInt        -> "INFint"
  | Void          -> "void"
  | NUM          -> "NUM"
  | AnnT          -> "AnnT"
  | Tree_sh		  -> "Tsh"
  | BagT t        -> "bag_"^(string_of_typ t)
  | TVar t        -> "TVar_"^(string_of_int t)
  | List t        -> "list_"^(string_of_typ t)
  | RelT a      -> "RelT("^(pr_list string_of_typ a)^")"
  | Pointer t        -> "Pointer{"^(string_of_typ t)^"}"
  | HpT        -> "HpT"
  | Named ot -> if ((String.compare ot "") ==0) then "null" else ot
  | Array (et, r) -> (* An Hoa *)
	let rec repeat k = if (k == 0) then "" else "_arr" ^ (repeat (k-1)) in
		(string_of_typ et) ^ (repeat r)
;;

let subs_tvar_in_typ t (i:int) nt =
  let rec helper t = match t with
    | TVar j -> if i==j then nt else t
    | BagT et -> BagT (helper et)
    | List et -> List (helper et)
    | Array (et,d) -> Array (helper et,d)
    | _ -> t
  in helper t
;;

let null_type = Named ""
;;

let is_null_type t=
  match t with
    | Named "" -> true
    | _ -> false


let rec s_i_list l c = match l with 
  | [] -> ""
  | h::[] -> h 
  | h::t -> h ^ c ^ (s_i_list t c)
;;

let string_of_ident_list l = "["^(s_i_list l ",")^"]"
;;

let string_of_primed p =
  match p with
    | Primed -> "'"
    | Unprimed -> ""

let string_of_primed_ident (id,p) =
  id ^ string_of_primed p

let pr_ident_list = pr_list (fun (i,p) -> i^(string_of_primed p))

let rec s_p_i_list l c = match l with 
  | [] -> ""
  | h::[] -> string_of_primed_ident h
  | h::t -> (string_of_primed_ident h) ^ c ^ (s_p_i_list t c)
;;

let string_of_primed_ident_list l = "["^(s_p_i_list l ",")^"]"
;;

let is_substr s id =
  let len_s = String.length s in
  try
    let s_id = String.sub id 0 len_s in
    if (s = s_id) then true
    else false
  with _ -> false
;;
 
let is_dont_care_var id =
  if is_substr "#" id 
  then true
  (* else if is_substr "Anon_" id then true *)
  else false
;;

let idf (x:'a) : 'a = x
let idf2 v e = v 
let nonef v = None
let voidf e = ()
let voidf2 e f = ()
let somef v = Some v
let or_list = List.fold_left (||) false
let and_list = List.fold_left (&&) true

let push_opt_void_pair e = match e with
  | None -> None
  | Some s -> Some (s,()) 

let push_opt_val opt v = match opt with
  | None -> None
  | Some s -> Some (s, v)

let push_opt_val_rev opt v = match opt with
  | None -> None
  | Some s -> Some (v, s)

let no_pos1 = { Lexing.pos_fname = "";
				   Lexing.pos_lnum = 0;
				   Lexing.pos_bol = 0; 
				   Lexing.pos_cnum = 0 } 

let res_name = "res"
let null_name = "null"

let sl_error = "separation entailment"
let logical_error = "logical bug"
let fnc_error = "function call"
let lemma_error = "lemma"
let undefined_error = "undefined"
let timeout_error = "timeout"

let eres_name = "eres"


let self = "self"

let constinfinity = "ZInfinity"
let deep_split_disjuncts = ref false
let check_integer_overflow = ref false

let this = "this"

let is_self_ident id = self=id

let thread_name = "thread"  (*special thread id*)
let thread_typ = Int  (*special thread id*)
let proc_typ = Void  (*special thread id*)
let fork_name = "fork"  (*generic, its args can vary*)
let join_name = "join"

let init_name = "init"  (*generic, its args can vary*)
let finalize_name = "finalize"
let acquire_name = "acquire"
let release_name = "release"
let lock_name = "lock"
let lock_typ = Named "lock"

let ls_name = "LS"
let lsmu_name = "LSMU"
let ls_data_typ = "lock"

let waitlevel_name = "waitlevel"
let waitlevel_typ = Int

let level_pred = "level"
let level_name = "mu"
let level_data_typ = Int
let ls_typ = BagT (Named ls_data_typ)
let lsmu_typ = BagT (Int)

let silence_output = ref false

(*precluded files*)
let header_file_list  = ref (["\"prelude.ss\""] : string list)
let pragma_list = ref ([] : string list)
let lib_files = ref ([] : string list)

(*in case the option of saving provers temp files to a different directory is enabled, the value of 
  this variable is going to be changed accordingly in method set_tmp_files_path *)
(*let tmp_files_path = "/tmp/"*)

(* *GLOBAL_VAR* input filename, used by iparser.mly, astsimp.ml and main.ml
 * moved here from iparser.mly *)

(* command line options *)

let ptr_to_int_exact = ref false

let is_sleek_running = ref false

let remove_label_flag = ref false
let label_split_conseq = ref true
let label_split_ante = ref true
let label_aggressive_sat = ref true
let label_aggressive_imply = ref true

let texify = ref false
let testing_flag = ref false

let instantiation_variants = ref 0

let omega_simpl = ref true

let no_simpl = ref false

let source_files = ref ([] : string list)

let input_file_name =ref ""

let use_split_match = ref false

let consume_all = ref false

let enable_split_lemma_gen = ref false

let dis_show_diff = ref false

let sa_print_inter = ref false

let print_heap_pred_decl = ref true

let cond_path_trace = ref true

let pred_syn_modular = ref true

let syntatic_mode = ref false (* syntatic mode - default is semantic*)

let sa_dnc = ref false

let pred_reuse = ref false

(*temp: should be improve*)
let pred_en_oblg = ref true

(* let sa_en_norm = ref false *)

let pred_syn_flag = ref true

let sa_syn = ref true

let lemma_syn = ref false

let sa_en_split = ref false

let pred_split = ref false

(* let sa_dangling = ref false *)

let sa_refine_dang = ref false

let pred_elim_useless = ref false
let infer_deep_ante_flag = ref false

let pred_infer_flag = ref true

let pred_elim_dangling = ref false

(* let sa_inlining = ref false *)

let sa_sp_split_base = ref false
let sa_pure_field = ref false

let sa_infer_split_base = ref true

let pred_elim_unused_preds = ref true

(* let sa_keep_unused_preds = ref false *)

let sa_unify_dangling = ref false

let pred_conj_unify = ref false

let pred_disj_unify = ref false

let pred_equiv = ref false

let sa_tree_simp = ref false

let sa_subsume = ref false

(* let norm_elim_useless = ref false *)

let norm_extract = ref false
let allow_norm_disj = ref true

let norm_cont_analysis = ref true

let lemma_infer = ref false

let dis_sem = ref false

(* let show_diff_constrs = ref true *)

let procs_verified = ref ([] : string list)

let false_ctx_line_list = ref ([] : loc list)

let b_datan = "barrier"

let verify_callees = ref false

let elim_unsat = ref false
let disj_compute_flag = ref false
let compute_xpure_0 = ref true
let inv_wrap_flag = ref true
let lhs_case_flag = ref false
let lhs_case_search_flag = ref false
let smart_xpure = ref true
let super_smart_xpure = ref false
let precise_perm_xpure = ref true
  (* this flag is dynamically set depending on
     smart_xpure and xpure0!=xpure1 *)
let smart_memo = ref false

let enable_constraint_based_filtering = ref false

(* let lemma_heuristic = ref false *)

let elim_exists_ff = ref true

let allow_imm = ref true (*imm will delay checking guard conditions*)

let allow_imm_inv = ref true (* imm inv to add of form @M <: v <:@A *)

(*Since this flag is disabled by default if you use this ensure that 
run-fast-test mem test cases pass *)
let allow_field_ann = ref false 
  (* disabled by default as it is unstable and
     other features, such as shape analysis are affected by it *)

let allow_mem = ref false
(*enabling allow_mem will turn on field ann as well *)

let infer_mem = ref false

let pa = ref false

let allow_inf = ref false (*enable support to use infinity (\inf and -\inf) in formulas *)

let ann_derv = ref false

let print_ann = ref true
let print_derv = ref false

let print_clean_flag = ref false

(*is used during deployment, e.g. on a website*)
(*Will shorten the error/warning/... message delivered
to end-users*)
let is_deployed = ref false 

let print_assume_struc = ref false

let web_compile_flag = ref false (*enable compilation flag for website*)

(* Decide whether normalization/simplification
such as x<1 --> x+1<=1 is allowed
   Currently, =true when using -tp parahip|rm
   or using -perm frac
   The reason for this is that when using concurrency verification,
   (floating-point) permission  constraints could be related to
   integer constraints; therefore, this renders the normalization
   unsound.
   Look at example at sleekex/examples/fracperm/locks/bug-simplify.slk
   for more details.
   Currently, conservativly do not allow such simplification
*)
let allow_norm = ref false

let allow_ls = ref false (*enable lockset during verification*)

let allow_locklevel = ref false (*enable locklevel during verification*)

(* let has_locklevel = ref false *)

let ann_vp = ref false (* Disable variable permissions in default, turn on in para5*)

let allow_ptr = ref false (*true -> enable pointer translation*)

let print_proc = ref false

let check_all = ref true
  
let auto_number = ref true

let sleek_flag = ref false

let sleek_log_filter = ref true
(* flag to filter trivial sleek entailment logs *)
(* particularly child calls *)

let use_field = ref false

let large_bind = ref false

let print_x_inv = ref false
let print_cnv_null = ref false

let hull_pre_inv = ref false

let use_coercion = ref true

let case_split = ref false

let simplified_case_normalize = ref true

let use_set = ref true

let consistency_checking = ref false

let wrap_exist = ref false

let move_exist_to_LHS = ref false

let max_renaming = ref false

let anon_exist = ref true

let simplify_pure = ref false

let enable_norm_simp = ref false

let print_version_flag = ref false

let elim_exists_flag = ref true

let filtering_flag = ref true

let split_rhs_flag = ref true

let n_xpure = ref 1

let verbose_num = ref 0

let fixcalc_disj = ref 2

let pre_residue_lvl = ref 0
(* Lvl 0 - add conjunctive pre to residue only *) 
(* Lvl 1 - add all pre to residue *) 
(* Lvl -1 - never add any pre to residue *) 

let check_coercions = ref false

let num_self_fold_search = ref 0

let self_fold_search_flag = ref false

let show_gist = ref false
let imply_top_flag = ref false
let early_contra_flag = ref true

let trace_failure = ref false

let trace_all = ref false

let print_mvars = ref false

let print_type = ref false

(* let enable_sat_statistics = ref false *)

let wrap_exists_implicit_explicit = ref false

let profiling = ref false

let enable_syn_base_case = ref false

let enable_case_inference = ref false

let print_core = ref false
let print_core_all = ref false

let print_err_sleek = ref false

let enable_prune_cache = ref true

let enable_counters = ref false

let enable_time_stats = ref true

let enable_count_stats = ref true

let enable_fast_imply = ref false

let failure_analysis = ref false

let seq_to_try = ref false

let print_input = ref false
let print_input_all = ref false

let print_cil_input = ref false
(* let pass_global_by_value = ref true *)

(* let allow_pred_spec = ref false *)

let disable_failure_explaining = ref true

let simplify_error = ref false

let prune_cnt_limit = ref 2

let suppress_warning_msg = ref false
let disable_elim_redundant_ctr = ref false

let enable_strong_invariant = ref false
let enable_aggressive_prune = ref false
let enable_redundant_elim = ref false

let enable_constraint_based_filtering = ref false

(* let disable_aggressive_prune = ref false *)
(* let prune_with_slice = ref false *)

let enulalias = ref false

let pass_global_by_value = ref true

let exhaust_match = ref false

let memo_verbosity = ref 2

let profile_threshold = 0.5 

let no_cache_formula = ref false

let simplify_imply = ref true

let enable_incremental_proving = ref false

let disable_multiple_specs =ref false

let perm_prof = ref false

let cp_test = ref false 

let cp_prefile = ref false 

let gen_cpfile = ref false 

let file_cp = ref ""

let cpfile = ref ""

  (*for cav experiments*)
  let f_1_slice = ref false
  let f_2_slice = ref false
  let no_memoisation = ref false
  let no_incremental = ref false
  let no_LHS_prop_drop = ref false
  let no_RHS_prop_drop = ref false
  let do_sat_slice = ref false

(* for Termination *)
let dis_term_chk = ref false
let term_verbosity = ref 1
let dis_call_num = ref false
let dis_phase_num = ref false
let term_reverify = ref false
let dis_bnd_chk = ref false
let dis_term_msg = ref false
let dis_post_chk = ref false
let dis_ass_chk = ref false
let log_filter = ref true
  
(* Options for slicing *)
let en_slc_ps = ref false
let override_slc_ps = ref false (*used to force disabling of en_slc_ps, for run-fast-tests testing of modular examples*)
let dis_ps = ref false
let dis_slc_ann = ref false
let slicing_rel_level = ref 2

(* let do_slicing = ref false *)
let dis_slicing = ref false
let opt_imply = ref 0
let opt_ineq = ref false
let infer_slicing = ref false
let infer_lvar_slicing = ref false
let multi_provers = ref false
let is_sat_slicing = ref false
let delay_case_sat = ref false
let force_post_sat = ref false
let delay_if_sat = ref false
let delay_proving_sat = ref false
let disable_assume_cmd_sat = ref false
let disable_pre_sat = ref true

(* Options for invariants *)
let do_infer_inv = ref false

(** for classic frame rule of separation logic *)
let opt_classic = ref false                (* option --classic is turned on or not? *)
let do_classic_frame_rule = ref false      (* use classic frame rule or not? *)

(** for type of frame inference rule that will be used in specs commands *)
(* type = None       --> option --classic will be used to decides whether using classic rule or not? *)
(*        Some true  --> always perform classic rule, regardless of --classic option                 *)
(*        Some false --> always perform intutitive rule, regardless of --classic option              *)
type ensures_type = bool option
type assert_type = bool option
type entail_type = bool option

(* Options for abduction *)
let do_abd_from_post = ref false

(* Flag of being unable to fold rhs_heap *)
let unable_to_fold_rhs_heap = ref false

(* Used in parse_shape.ml *)
let domain_name = ref ""

(* Options for incremental spec *)
let do_infer_inc = ref false

(* Inference *)
(*let call_graph : ((string list) list) ref = ref [[]]*)

let add_count (t: int ref) = 
	t := !t+1

let omega_err = ref false

let seq_number = ref 10

let sat_timeout_limit = ref 2.
let imply_timeout_limit = ref 3.

let dis_provers_timeout = ref false
let sleek_timeout_limit = ref 0.
  
(* let reporter = ref (fun _ -> raise Not_found) *)

(* let report_error2 (pos : loc) (msg : string) = *)
(*   let _ = *)
(*     try !reporter pos msg *)
(*     with Not_found -> *)
(*       let report pos msg = *)
(*         let output = Printf.sprintf "\n%s:%d:%d: %s\n" *)
(*           pos.start_pos.Lexing.pos_fname *)
(*           pos.start_pos.Lexing.pos_lnum *)
(*           (pos.start_pos.Lexing.pos_cnum - pos.start_pos.Lexing.pos_bol) *)
(*           msg *)
(*         in *)
(*         print_endline output *)
(*       in *)
(*       reporter := report; *)
(*       report pos msg *)
(*   in *)
(*   failwith "Error detected" *)

let branch_point_id = ref 0

let reset_formula_point_id () = () (*branch_point_id:=0*)

let iast_label_table = ref ([]:(control_path_id*string*((control_path_id*path_label*loc) list)*loc) list)

let locs_of_path_trace (pt: path_trace): loc list =
  let eq_path_id pid1 pid2 = match pid1, pid2 with
    | Some _, None -> false
    | None, Some _ -> false
    | None, None -> true
    | Some (i1, s1), Some (i2, s2) -> i1 = i2
  in
  let path_label_list_of_id pid =
    let _, _, label_list, _ = List.find (fun (id, _, _ , _) -> eq_path_id pid id) !iast_label_table in
    label_list
  in
  let loc_of_label plbl ref_list =
    let _, _, loc = List.find (fun (_, lbl, _) -> plbl = lbl) ref_list in
    loc
  in
  let find_loc pid plbl = 
    let label_list = path_label_list_of_id pid in
    loc_of_label plbl label_list
  in
  List.map (fun (pid, plbl) -> find_loc (Some pid) plbl) pt

let locs_of_partial_context ctx =
  let failed_branches = fst ctx in
  let path_traces = List.map fst failed_branches in
  let loc_list_list = List.map locs_of_path_trace path_traces in
  List.flatten loc_list_list


let fresh_formula_label (s:string) :formula_label = 
	branch_point_id := !branch_point_id + 1;
	(!branch_point_id,s)
  
let fresh_branch_point_id (s:string) : control_path_id = Some (fresh_formula_label s)
let fresh_strict_branch_point_id (s:string) : control_path_id_strict = (fresh_formula_label s)

let eq_formula_label (l1:formula_label) (l2:formula_label) : bool = fst(l1)=fst(l2)

let fresh_int () =
  seq_number := !seq_number + 1;
  !seq_number

let seq_number2 = ref 0

let fresh_int2 () =
  seq_number2 := !seq_number2 + 1;
  !seq_number2

let reset_int2 () =
  seq_number2 := 0

let fresh_int () =
  seq_number := !seq_number + 1;
  !seq_number

let fresh_ty_var_name (t:typ)(ln:int):string = 
	("v_"^(string_of_typ_alpha t)^"_"^(string_of_int ln)^"_"^(string_of_int (fresh_int ())))

let fresh_var_name (tn:string)(ln:int):string = 
	("v_"^tn^"_"^(string_of_int ln)^"_"^(string_of_int (fresh_int ())))

let fresh_trailer () = 
  let str = string_of_int (fresh_int ()) in
  (*-- 09.05.2008 *)
	(*let _ = (print_string ("\n[globals.ml, line 103]: fresh name = " ^ str ^ "\n")) in*)
	(* 09.05.2008 --*)
    "_" ^ str

let fresh_any_name (any:string) = 
  let str = string_of_int (fresh_int ()) in
    any ^"_"^ str

let fresh_name () = 
  let str = string_of_int (fresh_int ()) in
    "f_r_" ^ str

let fresh_label pos = 
 (* let str = string_of_int (fresh_int ()) in*)
    "f_l_" ^ (string_of_int pos.start_pos.Lexing.pos_lnum)^"_"^(string_of_int (fresh_int ()))
	
let fresh_names (n : int) = (* number of names to be generated *)
  let names = ref ([] : string list) in
    for i = 1 to n do
      names := (fresh_name ()) :: !names
    done;
    !names

let formula_cache_no_series = ref 0

let fresh_formula_cache_no  () = 
  formula_cache_no_series := !formula_cache_no_series +1;
  !formula_cache_no_series

let gen_ext_name c1 c2 = "Ext~" ^ c1 ^ "~" ^ c2

let string_of_loc (p : loc) = 
  p.start_pos.Lexing.pos_fname ^ "_" ^ 
  (string_of_int p.start_pos.Lexing.pos_lnum) ^ ":" ^
  (string_of_int (p.start_pos.Lexing.pos_cnum-p.start_pos.Lexing.pos_bol)) ^ "_" ^
  (string_of_int p.end_pos.Lexing.pos_lnum) ^ ":" ^
  (string_of_int (p.end_pos.Lexing.pos_cnum-p.end_pos.Lexing.pos_bol))

let string_of_pos (p : Lexing.position) = "("^string_of_int(p.Lexing.pos_lnum) ^","^string_of_int(p.Lexing.pos_cnum-p.Lexing.pos_bol) ^")"
;;

let string_of_full_loc (l : loc) = "{"^(string_of_pos l.start_pos)^","^(string_of_pos l.end_pos)^"}";;

let string_of_loc_by_char_num (l : loc) = 
  Printf.sprintf "(%d-%d)"
    l.start_pos.Lexing.pos_cnum
    l.end_pos.Lexing.pos_cnum

let string_of_formula_label ((i,s):formula_label) =
      "(" ^ (string_of_int i) ^ " , " ^ s ^ ")"

let seq_local_number = ref 0

let fresh_local_int () =
  seq_local_number := !seq_local_number + 1;
  !seq_local_number

let fresh_local_var_name (tn : string) : string =
  tn ^ "_local_" ^ (string_of_int (fresh_local_int ()))

let join2 a b = (a,b)

let fst3 (x,_,_) = x

let snd3 (_,x,_) = x

let change_fst3 (_,b,c) a = (a,b,c)

let path_trace_eq p1 p2 =
  let rec eq pt1 pt2 = match pt1,pt2 with
    | [],[] -> true
    | [],xs -> false
    |  xs,[] -> false
    |  ((a1,_),b1)::zt1,((a2,_),b2)::zt2 -> a1=a2 && b1=b2 && (eq zt1 zt2)
  in eq (List.rev p1) (List.rev p2)

let path_trace_lt p1 p2 =
  let rec lt pt1 pt2 = match pt1,pt2 with
    | [],[] -> false
    | [],xs -> true
    | xs,[] -> false
    | ((a1,_),b1)::zt1,((a2,_),b2)::zt2 -> (a1<a2) || (a1=a2 && b1<b2) || (a1=a2 & b1=b2 && lt zt1 zt2)
  in lt (List.rev p1) (List.rev p2)

let path_trace_gt p1 p2 =
  let rec gt pt1 pt2 = match pt1,pt2 with
    | [],[] -> false
    | [],xs -> false
    |  xs,[] -> true
    | ((a1,_),b1)::zt1,((a2,_),b2)::zt2 -> (a1>a2) || (a1=a2 && b1>b2) || (a1=a2 & b1=b2 && gt zt1 zt2)
  in gt (List.rev p1) (List.rev p2)

 
let dummy_exception () = ()

(* convert a tree-like binary object into a list of objects *)
let bin_op_to_list (op:string)
  (fn : 'a -> (string * ('a list)) option) 
  (t:'a) : ('a list) =
  let rec helper t =
    match (fn t) with
      | None -> [t]
      | Some (op2, xs) -> 
          if (op=op2) then 
            List.concat (List.map helper xs)
          else [t]
  in (helper t)

let bin_to_list (fn : 'a -> (string * ('a list)) option) 
  (t:'a) : string * ('a list) =
  match (fn t) with
    | None -> "", [t]
    | Some (op, _) -> op,(bin_op_to_list op fn t)


(* An Hoa : option to print proof *)
let print_proof = ref false

(* Create a quoted version of a string, for example, hello --> "hello" *)
let strquote s = "\"" ^ s ^ "\""

let norm_file_name str =
	for i = 0 to (String.length str) - 1 do
		if str.[i] = '.' || str.[i] = '/' then str.[i] <- '_'
	done;
	str

(* let wrap_classic et f a = *)
(*   let flag = !do_classic_frame_rule in *)
(*   do_classic_frame_rule := (match et with *)
(*     | None -> !opt_classic *)
(*     | Some b -> b); *)
(*   try  *)
(*     let res = f a in *)
(*     (\* restore flag do_classic_frame_rule  *\) *)
(*     do_classic_frame_rule := flag; *)
(*     res *)
(*   with _ as e -> *)
(*       (do_classic_frame_rule := flag; *)
(*       raise e) *)

(* let wrap_gen save_fn set_fn restore_fn flags f a = *)
(*   (\* save old_value *\) *)
(*   let old_values = save_fn flags in *)
(*   let _ = set_fn flags in *)
(*   try  *)
(*     let res = f a in *)
(*     (\* restore old_value *\) *)
(*     restore_fn old_values; *)
(*     res *)
(*   with _ as e -> *)
(*       (restore_fn old_values; *)
(*       raise e) *)

(* let wrap_one_bool flag new_value f a = *)
(*   let save_fn flag = (flag,!flag) in *)
(*   let set_fn flag = flag := new_value in *)
(*   let restore_fn (flag,old_value) = flag := old_value in *)
(*   wrap_gen save_fn set_fn restore_fn flag f a *)

(* let wrap_two_bools flag1 flag2 new_value f a = *)
(*   let save_fn (flag1,flag2) = (flag1,flag2,!flag1,!flag2) in *)
(*   let set_fn (flag1,flag2) = flag1 := new_value; flag2:=new_value in *)
(*   let restore_fn (flag1,flag2,old1,old2) = flag1 := old1; flag2:=old2 in *)
(*   wrap_gen save_fn set_fn restore_fn (flag1,flag2) f a *)

(* (\* let wrap_general flag new_value f a = *\) *)
(* (\*   (\\* save old_value *\\) *\) *)
(* (\*   let old_value = !flag in *\) *)
(* (\*   flag := new_value; *\) *)
(* (\*   try  *\) *)
(* (\*     let res = f a in *\) *)
(* (\*     (\\* restore old_value *\\) *\) *)
(* (\*     flag := old_value; *\) *)
(* (\*     res *\) *)
(* (\*   with _ as e -> *\) *)
(* (\*       (flag := old_value; *\) *)
(* (\*       raise e) *\) *)

(* let wrap_no_filtering f a = *)
(*   wrap_one_bool filtering_flag false f a *)

(* let wrap_lbl_dis_aggr f a = *)
(*   wrap_two_bools label_aggressive_sat label_aggressive_imply false f a *)

let proof_no = ref 0

let next_proof_no () =
  proof_no := !proof_no + 1;
  !proof_no

(* let next_proof_no_str () = *)
(*   proof_no := !proof_no + 1; *)
(*   string_of_int !proof_no *)

let get_proof_no () = !proof_no

let get_proof_no_str () = string_of_int !proof_no

let sleek_proof_no = ref 0

let last_sleek_fail_no = ref 0

let get_sleek_no () = !sleek_proof_no

let set_sleek_no n = sleek_proof_no:=n

let get_last_sleek_fail () = !last_sleek_fail_no

let set_last_sleek_fail () = 
  last_sleek_fail_no := !sleek_proof_no

(* let next_sleek_int () : int = *)
(*   sleek_proof_no := !sleek_proof_no + 1;  *)
(*   (!sleek_proof_no) *)


(* let read_from_debug_file chn : string list = *)
(*   let line = ref [] in *)
(*   let quitloop = ref false in *)
(*   (try *)
(*     while true do *)
(*       let xs = (input_line chn) in *)
(*       let n = String.length xs in *)
(*       (\* let s = String.sub xs 0 1 in *\) *)
(*       if n > 0 && xs.[0]=='#' (\* String.compare s "#" !=0 *\) then begin *)
(*         line := xs::!line; *)
(*       end; *)
(*     done; *)
(*   with _ -> ()); *)
(*   !line *)

(* let debug_map = Hashtbl.create 50 *)

(* let read_main () = *)
(*   let xs = read_from_debug_file (debug_file ()) in *)
(*   (\* let _ = print_endline ((pr_list (fun x -> x)) xs) in *\) *)
(*   List.iter (fun x -> *)
(*       try *)
(*         let l = String.index x ',' in *)
(*         let m = String.sub x 0 l in *)
(*         let split = String.sub x (l+1) ((String.length x) -l -1) in *)
(*         let _ = print_endline (m) in *)
(*         let _ = print_endline (split) in *)
(*         let kind = if String.compare split "Trace" == 0 then DO_Trace else *)
(*           if String.compare split "Loop" == 0 then DO_Loop else *)
(*             DO_Normal *)
(*         in *)
(*         Hashtbl.add debug_map m kind *)
(*       with _ -> *)
(*       Hashtbl.add debug_map x DO_Normal *)
(*   ) xs *)

(* let in_debug x = *)
(*   try *)
(*     Hashtbl.find debug_map x *)
(*   with _ -> DO_None *)




