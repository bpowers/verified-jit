{
  open Lexing
  open BytecodeParse
}

let int = ['0'-'9']+

rule token = parse
  | "p" { POP }
  | "-" { SUB }
  | "s" { SWAP }
  | "c" { PUSH }
  | "j" { JUMP }
  | "=" { JEQ }
  | "<" { JLT }
  | "." { STOP }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
