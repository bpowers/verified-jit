open Core.Std
open Syntax

let max_stack_depth = 32

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

let eval (bytecode: string) (xs: int list) : int option =
  let l = max_stack_depth - (List.length xs) in
  let p = 0 in
  let cs = parse bytecode in
  let (stack, _, _, _) = Semantics.exec xs l p cs in
  let x86_instrs = X86.encode cs 0 cs in
  let open Printf in
  printf "prog: %s\n" (Syntax.show_prog cs);
  printf "x86 instructions: [\n";
  List.iter x86_instrs (fun i -> printf "  %s\n" (X86.show_instr i));
  printf "]\n";
  printf "x86 bytes: [\n ";
  List.iter (X86.to_bytes x86_instrs) (fun i -> printf " 0x%02x" (Char.to_int i));
  printf "\n]\n";

  (* let open Printf in *)
  (* printf "stack: ["; *)
  (* List.iter stack (printf "%d "); *)
  (* printf "]\n"; *)
  (* printf "enc: %s (orig: %s)\n" (encode (parse bytecode)) bytecode; *)
  match stack with
  | result :: _ -> Some result
  | _ -> None

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
                              | Some result -> printf "Result: %d\n" result
                              | None        -> printf "Executed fully, but nothing on stack."))

let () =
  Command.run ~version:"0.1.0" main
