open Core.Std
open Ctypes
open Foreign

type code = unit ptr
let code : code typ = ptr void

type stack = int ptr
let stack : code typ = ptr void

(* convert the error code returned from C to an OCaml variant *)
let is_error (n: int): Problem.t option =
  let open Problem in
  match n with
  | -999 -> Some NoCode
  | -998 -> Some BadCodeLen
  | -997 -> Some NoStack
  | -996 -> Some MmapFailed
  | -995 -> Some MprotectFailed
  | _    -> None

let jit_exec =
  foreign "jit_exec" (code @-> int @-> stack @-> returning int)

let exec (instrs: X86.instr list) (xs: int list) (max_stack_depth: int): (int, Problem.t) Result.t =
  let instrs_arr = CArray.of_list char (X86.to_bytes instrs) in
  let instrs_ptr = to_voidp (CArray.start instrs_arr) in
  let stack_arr = CArray.make int max_stack_depth ~initial:0 in
  List.iteri xs ~f:(fun i v -> CArray.set stack_arr (max_stack_depth - 1 - i) v);
  let stack_ptr = to_voidp ((CArray.start stack_arr) +@ (max_stack_depth - List.length xs)) in
  let result = jit_exec instrs_ptr (CArray.length instrs_arr) stack_ptr in
  match is_error result with
  | Some err -> Error err
  | None -> Ok result
