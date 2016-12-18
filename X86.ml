open Syntax

(** CPU registers *)
type reg =
  | EAX
  | EDX
  | EDI
  | EIP
[@@deriving show]

(** CPU status bits *)
type eflags =
  | CF
  | PF
  | AF
  | ZF
  | SF

(** memory permissions *)
type perm =
  | R
  | W
  | X

(** constant *)
type imm = int
[@@deriving show]

type rm =
  | Reg of reg
  | Mem of int
[@@deriving show]

type cond =
  | ALWAYS (* used for unconditional relative jumps *)
  | E (* E = equal *)
  | B (* B = below *)
[@@deriving show]
  (* full conditions supported by x86: *)
  (* | E | NE *)
  (* | S | NS *)
  (* | A | NA *)
  (* | B | NB *)

type dest_src =
  | RM_I of rm * imm
  | RM_R of rm * reg
  | R_RM of reg * rm
[@@deriving show]

type binop =
  | Mov
  | Add
  | Sub
  | Cmp
[@@deriving show]

type instr =
  | Binop of binop * dest_src
  | Xchg of rm * reg
  | Jcc of cond * imm (* jump based on a condition code, include jmp rel *)
  | Jmp of rm         (* excludes relative jumps *)
[@@deriving show]

let w2w (w: int): char =
  Char.chr (w land 0xff)

let ximm (w: int): char list =
  [w2w w; w2w (w asr 8); w2w (w lsr 16); w2w (w lsr 24)]

let instr_to_bytes (i: instr) : char list =
  match i with
  | Binop (Mov, R_RM (EAX, Reg EDI)) -> ['\x8B'; '\x07']
  | Binop (Mov, RM_R (Reg EDI, EAX)) -> ['\x89'; '\x07']
  | Binop (Mov, RM_I (Reg EAX, imm)) -> ['\xB8'] @ (ximm imm)
  | Binop (Add, RM_I (Reg EDI, imm)) -> if imm > 255;
                                        then failwith "ADD RM_I immediate too big"
                                        else ['\x83'; '\xC7'; w2w imm]
  | Binop (Cmp, R_RM (EAX, Reg EDI)) -> ['\x3B'; '\x07']
  | Binop (Sub, R_RM (EAX, Reg EDI)) -> ['\x2B'; '\x07']
  | Binop (Sub, RM_I (Reg EDI, imm)) -> if imm > 255;
                                        then failwith "SUB RM_I immediate too big"
                                        else ['\x83'; '\xEF'; w2w imm]
  | Xchg (Reg EDI, EAX)              -> ['\x87'; '\x07']
  | Jcc (ALWAYS, imm)                -> ['\xE9'] @ (ximm imm)
  | Jcc (E, imm)                     -> ['\x0F'; '\x84'] @ (ximm imm)
  | Jcc (B, imm)                     -> ['\x0F'; '\x82'] @ (ximm imm)
  | Jmp (Reg EDX)                    -> ['\xFF'; '\xE2']
  | _ -> failwith (Printf.sprintf "don't know how to encode %s" (show_instr i))

let instr_len (instr : instr): int =
  instr_to_bytes instr |> List.length

let to_bytes (instrs: instr list): char list =
  List.map instr_to_bytes instrs |> List.flatten

let rec instr_off_for_byte_off (instrs : instr list) (off : int): int =
  match off, instrs with
  | (0, _) -> 0
  | (i, _) when i < 0 -> failwith (Printf.sprintf "bad offset %d" i)
  | (_, []) -> failwith "end of instructions"
  | (_, instr :: instrs) -> let len = instr_len instr in
                            1 + instr_off_for_byte_off instrs (off - len)

let rec encode' (orig_cs: prog) (bytes_off: int) (cs: prog): instr list =
  let jump_len = 5 in (* jmp w/ relative offset is 5 bytes long *)
  let condjump_len = 8 in (* cmp is 2 bytes, jcc is 6 bytes *)
  let rec xenc_length c =
    List.length (to_bytes (xenc (fun x -> 0) c))
  and xenc (t: int -> int) (c: Syntax.instr): instr list =
    match c with
    | Pop    -> [Binop (Mov, R_RM (EAX, Reg EDI));
                 Binop (Add, RM_I (Reg EDI, 4))]
    | Sub    -> [Binop (Sub, R_RM (EAX, Reg EDI))]
    | Swap   -> [Xchg (Reg EDI, EAX)]
    | Push i -> [Binop (Sub, RM_I (Reg EDI, 4));
                 Binop (Mov, RM_R (Reg EDI, EAX));
                 Binop (Mov, RM_I (Reg EAX, i))]
    | Jump i -> [Jcc (ALWAYS, (t i) - bytes_off - jump_len)]
    | Jeq i  -> [Binop (Cmp, R_RM (EAX, Reg EDI));
                 Jcc (E, (t i) - bytes_off - condjump_len)]
    | Jlt i  -> [Binop (Cmp, R_RM (EAX, Reg EDI));
                 Jcc (B, (t i) - bytes_off - condjump_len)]
    | Stop   -> [Jmp (Reg EDX)]
  in
  let rec addr cs a p: int =
    match (cs, a, p) with
    | _, a, 0       -> a
    | [], a, _      -> a
    | c :: cs, a, p -> addr cs (a + xenc_length c) (p - 1)
  in
  match cs with
  | [] -> []
  | c :: cs -> let instrs = xenc (addr orig_cs 0) c in
               instrs @ (encode' orig_cs (bytes_off + (List.length (to_bytes instrs))) cs)

let encode (cs: prog): instr list =
    encode' cs 0 cs
