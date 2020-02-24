%{
  #include <stdio.h>
  #include "ast.h"

  int yylex(void);
  void yyerror(const char *s) {
    fprintf(stderr, "%s\n", s);
  }
%}

%union {
  int lit_value;
  char *ident;
  struct expr* e;
  struct expr_vect* e_ve;
}

%token <ident> IDENTIFIER
%token <lit_value> VAL
%token LET_KW IN_KW VAR_KW
%token IF_KW THEN_KW ELSE_KW
%token WHILE_KW DO_KW
%token LIT_TRUE LIT_FALSE
%token AND_SC AND OR_SC OR

%type <e> expr
%type <e_ve> vect_elem
%type <e_ve> vect_elem_cont

%right DO_KW
%right ELSE_KW
%right IN_KW
%nonassoc '['
%nonassoc ASSIGN_OP
%nonassoc IDENTIFIER
%nonassoc '('
%left AND_SC AND OR_SC OR
%nonassoc '<' '>' LE GE '=' NE
%left '+' '-'
%left '*' '/'
%nonassoc '!'

%%

program: program expr '\n' 
         {
           jit_eval($2);
           free_expr($2);
         }
       | %empty
       ;

expr: VAL         { $$ = make_val($1); }
    | LIT_TRUE    { $$ = make_bool(1); }
    | LIT_FALSE   { $$ = make_bool(0); }
    | IDENTIFIER  { $$ = make_identifier($1); }
    
    | LET_KW IDENTIFIER '=' expr IN_KW expr    { $$ = make_let($2, $4, $6); }
    | VAR_KW IDENTIFIER '=' expr IN_KW expr    { $$ = make_var($2, $4, $6); }
    | IDENTIFIER ASSIGN_OP expr                { $$ = make_assign($1, $3); }
    
    | IDENTIFIER '(' expr ')'    { $$ = make_call($1, $3); }
    
    | IF_KW expr THEN_KW expr ELSE_KW expr    { $$ = make_if($2, $4, $6); }

    | WHILE_KW expr DO_KW expr                { $$ = make_while($2, $4); }

    | '!' expr          { $$ = make_un_op('!', $2); }
    | expr '+' expr     { $$ = make_bin_op($1, '+', $3); }
    | expr '*' expr     { $$ = make_bin_op($1, '*', $3); }
    | expr '-' expr     { $$ = make_bin_op($1, '-', $3); }
    | expr '/' expr     { $$ = make_bin_op($1, '/', $3); }

    | expr '<' expr     { $$ = make_bin_op($1, '<', $3); }
    | expr '>' expr     { $$ = make_bin_op($1, '>', $3); }
    | expr LE  expr     { $$ = make_bin_op($1, LE, $3); }
    | expr GE  expr     { $$ = make_bin_op($1, GE, $3); }
    | expr '=' expr     { $$ = make_bin_op($1, '=', $3); }
    | expr NE  expr     { $$ = make_bin_op($1, NE, $3); }

    | expr AND_SC expr    { $$ = make_bin_op($1, AND_SC, $3); }
    | expr AND expr       { $$ = make_bin_op($1, AND   , $3); }
    | expr OR_SC expr     { $$ = make_bin_op($1, OR_SC , $3); }
    | expr OR expr        { $$ = make_bin_op($1, OR    , $3); }

    | '[' vect_elem ']'                   { $$ = make_vect($2); }
    | expr '[' expr ']'                   { $$ = make_vect_access_op($1, $3); }
    | expr '[' expr ']' ASSIGN_OP expr    { $$ = make_vect_update_op($1, $3, $6); }


    | '(' expr ')'    { $$ = $2; }
    | '\n' expr       { $$ = $2; }



vect_elem: expr vect_elem_cont             { $$ = make_expr_vect($1, $2); }
vect_elem_cont: ',' expr vect_elem_cont    { $$ = make_expr_vect($2, $3); }
              | %empty                     { $$ = NULL; }

%%

int main(void) 
{
  yyparse();

  return 0;
}
