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
  (* full conditions supported by x86: *)
  (* | E | NE *)
  (* | S | NS *)
  (* | A | NA *)
  (* | B | NB *)
[@@deriving show]

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
  [w2w w; w2w (w lsr 8); w2w (w lsr 16); w2w (w lsr 24)]

let instr_to_string (i: instr) : char list =
  match i with
  | Binop (Mov, R_RM (EAX, Reg EDI)) -> ['\x8B'; '\x07']
  | Binop (Add, RM_I (Reg EDI, imm)) -> if imm > 255;
                                        then failwith "ADD RM_I immediate too big"
                                        else ['\x83'; '\xC7'; w2w imm]
  | Binop (Cmp, R_RM (EAX, Reg EDI)) -> ['\x3B'; '\x07']
  | Binop (Sub, R_RM (EAX, Reg EDI)) -> ['\x2B'; '\x07']
  | Xchg (Reg EDI, EAX)              -> ['\x87'; '\x07']
  | Jcc (ALWAYS, imm)                -> ['\xe9'] @ (ximm imm)
  | Jcc (E, imm)                     -> ['\x0F'; '\x84'] @ (ximm imm)
  | Jcc (B, imm)                     -> ['\x0F'; '\x82'] @ (ximm imm)
  | Jmp (Reg EDX)                    -> ['\xFF'; '\xE2']
  | _ -> failwith (Printf.sprintf "don't know how to encode %s" (show_instr i))

let rec to_string (instrs: instr list): char list =
  List.map instr_to_string instrs |> List.flatten

let rec encode (a: int) (cs: prog): instr list =
  (* FIXME: needs to be length of bytes, not # of x86 instructions *)
  let rec xenc_length c =
    List.length (to_string (xenc (fun x -> 0) c))
  and xenc (t: int -> int) (c: Syntax.instr): instr list =
    match c with
    | Pop    -> [Binop (Mov, R_RM (EAX, Reg EDI));
		 Binop (Add, RM_I (Reg EDI, 4))]
    | Sub    -> [Binop (Sub, R_RM (EAX, Reg EDI))]
    | Swap   -> [Xchg (Reg EDI, EAX)]
    | Push i -> [Binop (Sub, RM_I (Reg EDI, 4));
		 Binop (Mov, RM_R (Reg EDI, EAX));
		 Binop (Mov, RM_I (Reg EAX, i))]
    | Jump i -> [Jcc (ALWAYS, t i)]
    | Jeq i  -> [Binop (Cmp, R_RM (EAX, Reg EDI));
		 Jcc (E, t i)]
    | Jlt i  -> [Binop (Cmp, R_RM (EAX, Reg EDI));
		 Jcc (B, t i)]
    | Stop   -> [Jmp (Reg EDX)]
  in
  let rec addr cs a p: int =
    match (cs, a, p) with
    | _, a, 0       -> a
    | [], a, _      -> a
    | c :: cs, a, p -> addr cs (a + xenc_length c) (p - 1)
  in
  List.map (fun c -> xenc (addr cs a) c) cs |> List.flatten
