open Core.Std
open Smtlib

let z3_path = "./z3"

(* A precondition or postcondition term, along with anciliary terms
   that also need to be verified, generated as a result of having
   while loops and if statements. *)
type cnd = Cnd of term * term list

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

let rec strongest_postcondition (solver : Smtlib.solver) (instr : X86.instr) (pc : int) (pre : edi): unit =
  Printf.printf "sp: %s\n" (X86.show_instr instr);
  let open X86 in
  match instr with
  | Binop (Add, RM_I (Reg EDI, imm)) -> assert_ solver (equals (const (edi_id pc)) (add (const (edi_id pre)) (Int imm)));
                                        assert_ solver (lt (const (edi_id pc)) (const "stack_max"));
  | Binop (Sub, RM_I (Reg EDI, imm)) -> assert_ solver (equals (const (edi_id pc)) (sub (const (edi_id pre)) (Int imm)));
                                        assert_ solver (lt (const (edi_id pc)) (const "stack_max"));
  | Binop (Mov, R_RM (EAX, Reg EDI)) (* affects value of stack, not contents *)
  | Binop (Mov, RM_R (Reg EDI, EAX)) (* affects value of stack, not contents *)
  | Binop (Mov, RM_I (Reg EAX, _))   (* changes value of top-of-stack, not size *)
  | Binop (Cmp, R_RM (EAX, Reg EDI)) (* sets eflags *)
  | Binop (Sub, R_RM (EAX, Reg EDI)) (* affects value of stack, not contents*)
  | Xchg (Reg EDI, EAX)              (* affects value of stack, not contents*)
  | Jcc (ALWAYS, _)
  | Jcc (E, _)
  | Jcc (B, _)
  | Jmp (Reg EDX)                    -> assert_ solver (equals (const (edi_id pc)) (const (edi_id pre)));
  | _ -> failwith (Printf.sprintf "don't know how to check %s" (show_instr instr))


let verify_path (solver : Smtlib.solver)
                (instrs: X86.instr list)
                (xs : int list)
                (max_stack_depth : int)
                (pcs : int list): (unit, Problem.t) Result.t =
  Printf.printf "path: [";
  List.iter ~f:(fun (i : int) -> Printf.printf "%d; " i) pcs;
  Printf.printf "]\n";
  let stack_base_id = (Id "stack_base") in
  let stack_max_id = (Id "stack_max") in
  push solver;
  (* declare_const solver (Id "ret_pc") int_sort; *)
  declare_const solver stack_base_id int_sort;
  declare_const solver stack_max_id int_sort;
  declare_const solver (Id (edi_id (-1))) int_sort;
  assert_ solver (equals (const (edi_id (-1))) (add (const "stack_base") (mul (Int (List.length xs)) (Int 4))));
  (* pre-edx = stack_base - 4*len(xs) *)
  pcs |> List.iteri
    ~f:(fun i pc ->
        if not (List.take pcs (i-1) |> List.exists ~f:(fun n -> n = pc)) then
          declare_const solver (Id (edi_id pc)) int_sort);
  pcs |> List.iteri
    ~f:(fun i pc ->
        let instr = List.nth instrs i |> Option.value_exn in
        let pre = List.nth pcs (i-1) |> Option.value ~default:(-1) in
        strongest_postcondition solver instr pc pre);
  (* constrain stack max depending on the base *)
  assert_ solver (equals (add (const "stack_base") (mul (Int max_stack_depth) (Int 4))) (const "stack_max"));
  assert_ solver (gt (const "stack_base") (Int 0));
  let sat = check_sat solver in
  let mdl = get_model solver in
  pop solver;
  (* setup precondition (relate xs + max_stack_depth to value in edi *)
  (* setup postcondition pc = edx, edi not overflowed/underflowed *)
  (* pass both to recursive verify *)
  Error (Problem.VerificationFailed "not implemented")

let stack_maintenance (instrs : X86.instr list)
                      (xs : int list)
                      (max_stack_depth : int): (unit, Problem.t) Result.t =
  let solver = Smtlib.make_solver z3_path in
  all_paths instrs 0 []
  |> List.map ~f:List.rev
  |> List.map ~f:(verify_path solver instrs xs max_stack_depth)
  |> List.fold_left ~init:(Ok ()) ~f:(fun r ok -> match r with
                                                  | Error err -> Error err
                                                  | _ -> ok)
