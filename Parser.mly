%{

open Syntax

%}

%token POP
%token SUB
%token SWAP
%token PUSH
%token JUMP
%token JEQ
%token JLT
%token STOP
%token <int> INT
%token EOF

%start program
%type <Syntax.prog> program

%%

int :
  | INT { $1 }

instr :
  | POP      { Pop }
  | SUB      { Sub }
  | SWAP     { Swap }
  | PUSH int { Push $2 }
  | JUMP int { Jump $2 }
  | JEQ int  { Jeq $2 }
  | JLT int  { Jlt $2 }
  | STOP     { Stop }

instructions :
  | instr instructions { $1 :: $2 }
  | { [] }

program :
  | instructions EOF { $1 }
%%
