open Core.Std

let stack_maintenance (instrs : X86.instr list)
                      (xs : int list)
                      (max_stack_depth : int): (unit, Problem.t) Result.t =
    Ok ()
