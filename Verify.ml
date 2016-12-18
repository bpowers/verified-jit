open Core.Std

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

let verify_path (instrs: X86.instr list)
                (xs : int list)
                (max_stack_depth : int)
                (pcs : int list): (unit, Problem.t) Result.t =
  Printf.printf "path: [";
  List.iter ~f:(fun (i : int) -> Printf.printf "%d; " i) pcs;
  Printf.printf "]\n";
  Error (Problem.VerificationFailed "not implemented")

let stack_maintenance (instrs : X86.instr list)
                      (xs : int list)
                      (max_stack_depth : int): (unit, Problem.t) Result.t =
  all_paths instrs 0 []
  |> List.map ~f:List.rev
  |> List.map ~f:(verify_path instrs xs max_stack_depth)
  |> List.fold_left ~init:(Ok ()) ~f:(fun r ok -> match r with
                                                  | Error err -> Error err
                                                  | _ -> ok)
