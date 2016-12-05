open Core.Std
open Ctypes
open Foreign

type code = unit ptr
let code : code typ = ptr void

type stack = unit ptr
let stack : code typ = ptr void

let jit_exec =
  foreign "jit_exec" (code @-> int @-> stack @-> returning int)

let exec (instrs: X86.instr list) (xs: int list) (max_stack_depth: int): int =
  let instrs_arr = CArray.of_list char (X86.to_bytes instrs) in
  let instrs_ptr = to_voidp (CArray.start instrs_arr) in
  let stack_arr = CArray.make int max_stack_depth ~initial:0 in
  List.iteri xs (fun i v -> CArray.set stack_arr (max_stack_depth - 1 - i) v);
  let stack_ptr = to_voidp ((CArray.start stack_arr) +@ (max_stack_depth - List.length xs)) in
  jit_exec instrs_ptr (CArray.length instrs_arr) stack_ptr
