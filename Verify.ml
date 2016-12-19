open Core.Std
open Smtlib

let z3_path = "./z3"


(* variable definition that matches what Z3 returns in the case of a
   satisfiable model *)
type def = Def of string * int

(* Similar to how exec/next in Semantics represent the PC as an
   int, we represent paths as sequences of offsets into instrs *)
let rec all_paths (instrs : X86.instr list) (pc: int) (curr : int list): int list list =
  let open X86 in
  let instr = match List.nth instrs pc with
    | Some instr -> instr
    | None -> failwith "unreachable: bad instr index"
  in
  let decode_target imm =
    let pc_byte_off = (List.take instrs (pc+1)) |> X86.to_bytes |> List.length in
    (* imm is a relative offset from the end of this instruction *)
    let abs_offset = (pc_byte_off + imm) in
    X86.instr_off_for_byte_off instrs abs_offset
  in
  match instr with
  | Jcc (ALWAYS, imm) -> let curr = pc :: curr in
                         let target = decode_target imm in
                         if List.mem curr target
                         then [target :: curr]
                         else all_paths instrs target curr
  | Jcc (cond, imm)   -> let curr = pc :: curr in
                         let target = decode_target imm in
                         let lbranch = all_paths instrs (pc + 1) curr in
                         let rbranch = if List.mem curr target
                                       then [target :: curr]
                                       else all_paths instrs target curr
                         in
                         List.append lbranch rbranch
  | Jmp (reg)         -> [pc :: curr] (* from Stop instruction *)
  | _                 ->  pc :: curr |> all_paths instrs (pc + 1)

type edi = int

let edi_id (i : edi) =
  Printf.sprintf "edi%d" i

let rec strongest_postcondition (solver : Smtlib.solver) (instr : X86.instr) (pc : int) (pre : edi): term list =
  let open X86 in
  match instr with
  | Binop (Add, RM_I (Reg EDI, imm)) -> assert_ solver (equals (const (edi_id pc)) (add (const (edi_id pre)) (Int imm)));
                                        [(lte (const (edi_id pc)) (const "stack_top"))] (* pop *)
  | Binop (Sub, RM_I (Reg EDI, imm)) -> assert_ solver (equals (const (edi_id pc)) (sub (const (edi_id pre)) (Int imm)));
                                        [(gte (const (edi_id pc)) (const "stack_bottom"))]
  | Binop (Mov, R_RM (EAX, Reg EDI)) (* affects value of stack, not contents *)
  | Binop (Mov, RM_R (Reg EDI, EAX)) (* affects value of stack, not contents *)
  | Binop (Mov, RM_I (Reg EAX, _))   (* changes value of top-of-stack, not size *)
  | Binop (Cmp, R_RM (EAX, Reg EDI)) (* sets eflags *)
  | Binop (Sub, R_RM (EAX, Reg EDI)) (* affects value of stack, not contents*)
  | Xchg (Reg EDI, EAX)              (* affects value of stack, not contents*)
  | Jcc (ALWAYS, _)
  | Jcc (E, _)
  | Jcc (B, _)                       -> assert_ solver (equals (const (edi_id pre)) (const (edi_id pc))); []
  | Jmp (Reg EDX)                    -> assert_ solver (equals (const (edi_id pre)) (const (edi_id pc)));
                                        [(lte (const (edi_id pc)) (const "stack_top"));
                                         (gte (const (edi_id pc)) (const "stack_bottom"))]
  | _ -> failwith (Printf.sprintf "don't know how to check %s" (show_instr instr))


(* shouldn't this be in the stdlib somewhere? *)
let is_some (a : 'a option) = match a with | Some _ -> true | None -> false
let get (a : 'a option) = match a with | Some x -> x | None -> failwith "get of None"

(* All SInts returned by Z3 are nats, negatives are represented by (- $nat) :\ *)
let get_def (name : string) (sexp : sexp) : def option = match sexp with
  | SInt x  -> Some (Def (name, x))
  | SList (SSymbol "-" :: SInt x :: []) -> Some (Def (name, -x))
  | _ -> eprintf "WARNING: unexpected var shape: %s\n%!" (sexp_to_string sexp); None


(* If we are unable to prove validity (with an unsat), return the
   satisfying model that Z3 found *)
let get_model (solver : solver) : def list option =
  let fdef (sexp : sexp) : def option = match sexp with
    | SList (SSymbol "define-fun" :: SSymbol name :: (SList []) :: SSymbol "Int" :: x :: []) -> get_def name x
    | _ -> eprintf "WARNING: unexpected define-fun shape: %s\n%!" (sexp_to_string sexp); None
  in
  let resp = command solver (SList [SSymbol "get-model";]) in
  match resp with
  | SList (SSymbol "model" :: l)               ->
    let defs = List.map l ~f:fdef in
    let filtered = List.filter defs ~f:is_some in
    let defs =List.map filtered ~f:get in
    Some (defs)
  | SList (SSymbol "error" :: SString _ :: []) -> None
  | _                                          -> eprintf "WARNING: bad get-model response: %s\n%!" (sexp_to_string resp);
    None


let verify_path (solver : Smtlib.solver)
                (instrs: X86.instr list)
                (xs : int list)
                (max_stack_depth : int)
                (pcs : int list): (unit, Problem.t) Result.t =
  Printf.printf "path: [";
  List.iter ~f:(fun (i : int) -> Printf.printf "%d; " i) pcs;
  Printf.printf "]\n";
  let assert_not_or (terms : term list): unit =
    assert_ solver (not_ (List.fold_left ~init:(bool_to_term true) ~f:and_ terms))
  in
  push solver;
  declare_const solver (Id "stack_bottom") int_sort;
  declare_const solver (Id "stack_top") int_sort;
  declare_const solver (Id (edi_id (-1))) int_sort;
  pcs |> List.iteri
    ~f:(fun i pc ->
        if not (List.take pcs (i-1) |> List.exists ~f:(fun n -> n = pc))
        then declare_const solver (Id (edi_id pc)) int_sort);
  assert_ solver (equals
                    (const (edi_id (-1)))
                    (sub (const "stack_top")
                              (mul (Int ((List.length xs) - 1))
                                   (Int 4))));
  assert_ solver (equals
                    (const "stack_top")
                    (add (const "stack_bottom")
                         (mul (Int max_stack_depth)
                              (Int 4))));
  assert_ solver (gt (const "stack_bottom") (Int 0));
  pcs
  |> List.mapi
    ~f:(fun i pc ->
        let instr = List.nth instrs pc |> Option.value_exn in
        let pre = List.nth pcs (i-1) |> Option.value ~default:(-1) in
        strongest_postcondition solver instr pc pre)
  |> List.fold_left ~init:[] ~f:(fun a b -> List.append a b)
  |> assert_not_or;
  let sat = check_sat solver in
  (* let mdl = get_model solver in *)
  pop solver;
  match sat with
  | Sat     -> Error (Problem.VerificationFailed "stack over or underflow")
  | Unknown -> Error (Problem.VerificationFailed "unknown")
  | Unsat   -> Ok ()

let stack_maintenance (instrs : X86.instr list)
                      (xs : int list)
                      (max_stack_depth : int): (unit, Problem.t) Result.t =
  if List.length xs < 1
  then Error Problem.StackTooSmall
  else
    let solver = Smtlib.make_solver z3_path in
    all_paths instrs 0 []
    |> List.map ~f:List.rev
    |> List.map ~f:(verify_path solver instrs xs max_stack_depth)
    |> List.fold_left ~init:(Ok ()) ~f:(fun r ok -> match r with
                                                    | Error err -> Error err
                                                    | _ -> ok)
