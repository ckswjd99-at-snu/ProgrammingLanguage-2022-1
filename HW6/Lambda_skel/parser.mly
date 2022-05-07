/*
 * SNU 4190.310 Programming Languages (Spring 2022)
 * Lambda Calculus
 * 
 * Kyuyeon Park
 * kypark@ropas.snu.ac.kr
 */
  
%{
exception EmptyBinding
exception ParsingError
%}

%token APP
%token LAMBDA DOT
%token <string> ID
%token IN
%token EQUAL
%token LET
%token LP RP
%token EOF

%nonassoc IN
%nonassoc DOT
%left ID LP
%left APP
%right EQUAL
%right LAMBDA

%start program
%type <Lambda.lexp_let> program

%%

program: exp EOF{ $1 } 

exp : 
	| ID { Lambda.LVar ($1) }
	| LAMBDA ID DOT exp { Lambda.LLam ($2, $4) }
	| LP exp RP { $2 }
	| exp exp %prec APP{ Lambda.LApp ($1, $2) }
	| LET ID EQUAL exp IN exp { Lambda.Let ($2, $4, $6) }
	;

%%
