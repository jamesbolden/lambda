%token PACKAGE IMPORT WHERE DATA TYPE STRUCT CLASS INSTANCE CASE OF LET IN IF THEN ELSE AS WHILE DO TRY CATCH LAMBDA NIL UNIT ANNOT LPAREN RPAREN LBRACK RBRACK RIGHTARROW FATRIGHTARROW FORALL INFIXL INFIXR INFIX DOT COMMA WILDCARD BACKTICK DEF BAR TOPLEVEL ELLIPSES INDENT DEDENT SEPARATOR SEMICOLON EOF
%token <string> UPPERID LOWERID OP STRING
%token <int> PRIMINTEGER INTEGER
%token <float> PRIMFLOATING FLOATING
%token <char> PRIMCHAR CHAR

%nonassoc UPPERID LOWERID NIL UNIT LPAREN LBRACK INTEGER FLOATING CHAR STRING FORALL WILDCARD CASE LET IF LAMBDA PRIMINTEGER PRIMFLOATING PRIMCHAR WHILE TRY
%nonassoc OP RIGHTARROW DOT BAR
%nonassoc APP

%start <string Syntax.Top_level.t list> prog
%%

prog:
    | EOF                                                                                   { [] }
    | pkgsig prog1                                                                          { $2 }
    ;

pkgsig:
    | PACKAGE signature WHERE                                                               { Lambda_env.set_pkg $2 }
    ;

signature:
    | UPPERID                                                                               { $1 }
    | UPPERID DOT signature                                                                 { $1 ^ "." ^ $3 }
    ;

prog1:
    | EOF                                                                                   { [] }
    | prog1 top_level                                                                       { $2 :: $1 }
    ;

top_level:
    | INFIXL INTEGER op                                                                     { Syntax.Top_level.Infixl_decl ($2, $3) }
    | INFIXR INTEGER op                                                                     { Syntax.Top_level.Infixr_decl ($2, $3) }
    | INFIX INTEGER op                                                                      { Syntax.Top_level.Infix_decl ($2, $3) }
    | IMPORT signature                                                                      { Syntax.Top_level.Import_stmt $2 }
    | DATA typename LOWERID* WHERE INDENT cons DEDENT                                       { Syntax.Top_level.Data_defn ($2, $3, $6) }
    | STRUCT typename LOWERID* WHERE INDENT fields DEDENT                                   { Syntax.Top_level.Struct_defn ($2, $3, $6) }
    | TYPE typename LOWERID* DEF typ                                                        { Syntax.Top_level.Type_synonym ($2, $3, $5) }
    | var ANNOT typ                                                                         { Syntax.Top_level.Type_annot ($1, $3) }
    | klass                                                                                 { $1 }
    | instance                                                                              { $1 }
    | binding                                                                               { Syntax.Top_level.Binding $1 }
    ;

typename:
    | UPPERID                                                                               { $1 }
    | NIL                                                                                   { "[]" }
    ;

cons:
    | con                                                                                   { [$1] }
    | con SEPARATOR cons                                                                    { $1 :: $3 }
    ;

fields: cons                                                                                { $1 }

typ:
    | UPPERID                                                                               { Type.Type_con ($1, Type.Kind.Star) }
    | LOWERID                                                                               { Type.Type_var ($1, Type.Kind.Star) }
    | typ RIGHTARROW typ                                                   %prec RIGHTARROW { Type.Type_app (Type.Type_app (Type.basic_arrow_ty, $1), $3) }
    | FORALL LOWERID* DOT typ                                                               { Type.Type_forall ($2, $4) }
    | typapp                                                                                { $1 }
    | ty_ctx FATRIGHTARROW typ                                                              { Type.Type_qual ($1, $3) }
    | LPAREN typ RPAREN                                                                     { $2 }
    | LBRACK typ RBRACK                                                                     { Type.Type_app (basic_list_ty, $2) }
    ;

typapp:
    | typapp typ                                                                            { Type.Type_app ($1, $2) }
    ;

var:
    | LOWERID                                                                               { $1 }
    | LPAREN op1 RPAREN                                                                     { $2 }
    ;

op1:
    | OP                                                                                    { $1 }
    | BAR                                                                                   { "|" }
    ;

pattern_op:
    | OP                                                                                    { $1 }
    ;

op:
    | op1                                                                                   { $1 }
    | BACKTICK LOWERID BACKTICK                                                             { $2 }
    | BACKTICK UPPERID BACKTICK                                                             { $2 }
    ;

klass:
    | CLASS UPPERID LOWERID+ WHERE INDENT class_body DEDENT                                 { Syntax.Top_level.Class_defn ([], $2, $3, $6) }
    | CLASS class_ctx FATRIGHTARROW UPPERID LOWERID+ WHERE INDENT class_body DEDENT         { Syntax.Top_level.Class_defn ($2, $4, $5, $8) }
    ;

instance:
    | INSTANCE UPPERID typ+ WHERE INDENT inst_body DEDENT                                   { Syntax.Top_level.Inst_defn ([], $2, $3, $6) }
    | INSTANCE ty_ctx FATRIGHTARROW UPPERID typ+ WHERE INDENT inst_body DEDENT              { Syntax.Top_level.Inst_defn ($2, $4, $5, $8) }
    ;

binding:
    | var pattern* DEF exp                                                                  { Syntax.Binding.Normal_binding ($1, $2, None, $4, []) }
    | var pattern* BAR exp DEF exp                                                          { Syntax.Binding.Normal_binding ($1, $2, Some $4, $6, []) }
    | var pattern* DEF exp WHERE INDENT bindings DEDENT                                     { Syntax.Binding.Normal_binding ($1, $2, None, $4, $7) }
    | var pattern* BAR exp DEF exp WHERE INDENT bindings DEDENT                             { Syntax.Binding.Normal_binding ($1, $2, Some $4, $6, $9) }
    | pattern op pattern DEF exp                                                            { Syntax.Binding.Normal_binding ($2, [$1; $3], None, $5, []) }
    | pattern op pattern BAR exp DEF exp                                                    { Syntax.Binding.Normal_binding ($2, [$1; $3], Some $5, $7, []) }
    | pattern op pattern DEF exp WHERE INDENT bindings DEDENT                               { Syntax.Binding.Normal_binding ($2, [$1; $3], None, $5, $8) }
    | pattern op pattern BAR exp DEF exp WHERE INDENT bindings DEDENT                       { Syntax.Binding.Normal_binding ($2, [$1; $3], Some $5, $7, $10) }
    | pattern DEF exp                                                                       { Syntax.Binding.Pattern_binding ($1, None, $3, []) }
    | pattern BAR exp DEF exp                                                               { Syntax.Binding.Pattern_binding ($1, Some $3, $5, []) }
    | pattern DEF exp WHERE INDENT bindings DEDENT                                          { Syntax.Binding.Pattern_binding ($1, None, $3, $6) }
    | pattern BAR exp DEF exp WHERE INDENT bindings DEDENT                                  { Syntax.Binding.Pattern_binding ($1, Some $3, $5, $8) }
    ;

con:
    | var ANNOT typ                                                                         { $1, $3 }
    ;

ty_ctx:
    | UPPERID typ+                                                                          { [$1, $3] }
    | LPAREN ty_ctx1 RPAREN                                                                 { $2 }
    ;

class_body:
    | class_member                                                                          { [$1] }
    | class_member SEPARATOR class_body                                                     { $1 :: $3 }
    ;

class_ctx:
    | UPPERID LOWERID+                                                                      { [$1, $3] }
    | LPAREN class_ctx1 RPAREN                                                              { $2 }
    ;

inst_body:
    | binding                                                                               { [$1] }
    | binding SEPARATOR inst_body                                                           { $1 :: $3 }
    ;

pattern:
    | UNIT                                                                                  { Syntax.Pattern.Unit_pattern }
    | NIL                                                                                   { Syntax.Pattern.Nil_pattern }
    | WILDCARD                                                                              { Syntax.Pattern.Wildcard_pattern }
    | PRIMINTEGER                                                                           { Syntax.Pattern.Prim_int_pattern $1 }
    | PRIMCHAR                                                                              { Syntax.Pattern.Prim_char_pattern $1 }
    | INTEGER                                                                               { Syntax.Pattern.Con_pattern ("I#", [Syntax.Pattern.Prim_int_pattern $1]) }
    | CHAR                                                                                  { Syntax.Pattern.Con_pattern ("C#", [Syntax.Pattern.Prim_char_pattern $1]) }
    | STRING                                                                                { Syntax.Pattern.String_pattern $1 }
    | LOWERID                                                                               { Syntax.Pattern.Var_pattern $1 }
    | pattern AS LOWERID                                                                    { Syntax.Pattern.Alias_pattern ($1, $3) }
    | UPPERID pattern*                                                                      { Syntax.Pattern.Con_pattern ($1, $2) }
    | pattern pattern_op pattern                                                            { Syntax.Pattern.Con_pattern ($2, [$1; $3]) }
    | pattern BAR pattern                                                                   { Syntax.Pattern.Or_pattern ($1, $3) }
    | exp RIGHTARROW pattern                                                                { Syntax.Pattern.View_pattern ($1, $3) }
    | pattern ANNOT typ                                                                     { Syntax.Pattern.Constraint_pattern ($1, $3) }
    | LPAREN pattern RPAREN                                                                 { $2 }
    ;

exp:
    | UNIT                                                                                  { Syntax.Exp.Unit }
    | NIL                                                                                   { Syntax.Exp.Var "[]" }
    | PRIMINTEGER                                                                           { Syntax.Exp.Int $1 }
    | PRIMFLOATING                                                                          { Syntax.Exp.Float $1 }
    | PRIMCHAR                                                                              { Syntax.Exp.Char $1 }
    | INTEGER                                                                               { Syntax.Exp.App (Syntax.Exp.Var "I#", Syntax.Exp.Int $1) }
    | FLOATING                                                                              { Syntax.Exp.App (Syntax.Exp.Var "D#", Syntax.Exp.Float $1) }
    | CHAR                                                                                  { Syntax.Exp.App (Syntax.Exp.Var "C#", Syntax.Exp.Char $1) }
    | STRING                                                                                { Syntax.Exp.String $1 }
    | LOWERID                                                                               { Syntax.Exp.Var $1 }
    | UPPERID                                                                               { Syntax.Exp.Var $1 }
    | exp exp                                                                   %prec APP   { Syntax.Exp.App ($1, $2) }
    | exp op exp                                                                %prec OP    { Syntax.Exp.Infix ($2, $1, $3) }
    | LAMBDA pattern+ RIGHTARROW exp                                                        { Syntax.Exp.Lambda ($2, $4) }
    | LAMBDA CASE INDENT alts DEDENT                                                        { Syntax.Exp.Case_lambda $4 }
    | CASE exp OF INDENT alts DEDENT                                                        { Syntax.Exp.Case ($2, $5) }
    | IF exp THEN exp ELSE exp                                                              { Syntax.Exp.If ($2, $4, $6) }
    | WHILE exp DO exp                                                                      { Syntax.Exp.While ($2, $4) }
    | TRY exp CATCH INDENT alts DEDENT                                                      { Syntax.Exp.Try_catch ($2, $5) }
    | exp ANNOT typ                                                                         { Syntax.Exp.Constraint ($1, $3) }
    | exp DOT LOWERID                                                                       { Syntax.Exp.Field_access ($1, $3) }
    | exp DEF exp                                                                           { Syntax.Exp.Assignment ($1, $3) }
    | LET binding IN exp                                                                    { Syntax.Exp.Let ([$2], $4) }
    | LET INDENT inst_body DEDENT IN exp                                                    { Syntax.Exp.Let ($3, $6) }
    | LBRACK exps_semicolon RBRACK                                                          { Parse_aux.desugar_list $2 }
    | INDENT exps DEDENT                                                                    { Parse_aux.desugar_block $2 }
    | LPAREN exp RPAREN                                                                     { $2 }
    ;

exps:
    | exp                                                                                   { [$1] }
    | exp SEPARATOR exps                                                                    { $1 :: $3 }
    ;

clause:
    | binding                                                                               { [$1] }
    | binding SEPARATOR clause                                                              { $1 :: $3 }
    ;

ty_ctx1:
    | UPPERID typ+                                                                          { [$1, $2] }
    | UPPERID typ+ COMMA ty_ctx1                                                            { ($1, $2) :: $4 }
    ;

class_member:
    | var ANNOT typ                                                                         { Syntax.Class.Method_sig ($1, $3) }
    | binding                                                                               { Syntax.Class.Default_method $1 }
    ;

class_ctx1:
    | UPPERID LOWERID+                                                                      { [$1, $2] }
    | UPPERID LOWERID+ COMMA class_ctx1                                                     { ($1, $2) :: $4 }
    ;

alts:
    | pattern RIGHTARROW exp                                                                { [$1, $3] }
    | pattern RIGHTARROW exp SEPARATOR alts                                                 { ($1, $3) :: $5 }
    ;

exps_semicolon:
    | exp                                                                                   { [$1] }
    | exp SEMICOLON exps_semicolon                                                          { $1 :: $3 }
    ;
