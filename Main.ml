open Core.Std
open Syntax

let max_stack_depth = 32

let parse (bytecode: string) : prog =
  BytecodeParse.program BytecodeLexer.token (Lexing.from_string bytecode)

(* encodes a given program as an ASCII string *)
let rec encode (prog: prog) : string =
  let imm (i : int) =
    Printf.sprintf "%d" i
  in
  let enc (instr: instr) =
    match instr with
    | Pop    -> "p"
    | Sub    -> "-"
    | Swap   -> "s"
    | Push i -> "c" ^ imm i
    | Jump i -> "j" ^ imm i
    | Jeq i  -> "=" ^ imm i
    | Jlt i  -> "<" ^ imm i
    | Stop   -> "."
  in
  match prog with
  | [] -> ""
  | instr :: prog -> (enc instr) ^ (encode prog)

let debug_x86_instrs x86_instrs =
  printf "x86 instructions: [\n";
  x86_instrs |> List.iter ~f:(fun i -> printf "  %s\n" (X86.show_instr i));
  printf "]\n"

let debug_x86_bytes x86_instrs =
  printf "x86 bytes: [\n ";
  X86.to_bytes x86_instrs |> List.iter ~f:(fun i -> printf " 0x%02x" (Char.to_int i));
  printf "\n]\n"

let eval (bytecode: string) (xs: int list) : (int, Problem.t) Result.t =
  let program = parse bytecode in
  let instrs = X86.encode program in
  Printf.printf "prog: %s\n" (Syntax.show_prog program);
  debug_x86_instrs instrs;
  debug_x86_bytes instrs;
  match Verify.stack_maintenance instrs xs max_stack_depth with
  | Error err -> Error err
  | Ok ()     -> Jit.exec instrs xs max_stack_depth

let spec =
  let open Command.Spec in
  empty
  +> anon ("bytecode" %: string)
  +> anon (sequence ("number" %: int))

let main =
  Command.basic
    ~summary:"Run a bytecode program"
    ~readme:(fun () -> "JIT compiles and executes x86 instructions corresponding to the bytecode.")
    spec
    (fun bytecode args () -> (match eval bytecode args with
                              | Ok result -> printf "Result: %d\n" result
                              | Error err -> printf "JIT error: %s\n" (Problem.show err)))

let () =
  Command.run ~version:"0.1.0" main
