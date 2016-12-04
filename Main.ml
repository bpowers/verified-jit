open Core.Std
open Syntax

let parse (bytecode: string) : prog =
  Parser.program Lexer.token (Lexing.from_string bytecode)

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

let eval (bytecode: string) (args: int list) : int =
  printf "enc: %s (orig: %s)\n" (encode (parse bytecode)) bytecode;
  -1

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
    (fun bytecode args () -> (let open Printf in
			      printf "Top of stack: %d\n" (eval bytecode args)))

let () =
  Command.run ~version:"0.1.0" main
