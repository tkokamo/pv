{
module HOLParser where
import HOLCommon
import Data.Char
}

%name holparser
%tokentype { Token }
%error { parseError }

%token
     ';'                           { TKSEMICOLON }
     '.'                           { TKPERIOD }
     '('                             { TKLPAR }
     ')'                             { TKRPAR }
     '{'                             { TKLCUR }
     '}'                             { TKRCUR }
     '['                             { TKLBRA }
     ']'                             { TKRBRA }
     '+'                             { TKPLUS }
     '-'                            { TKMINUS }
     '*'                         { TKASTERISK }
     '/'                             { TKDIV }
     '%'                              { TKMOD }
     not                              { TKNOT }
     and                              { TKAND }
     or                               { TKOR }
     imp                              { TKIMP }
     equ                              { TKEQU }
     neq                              { TKNEQ }
     lth                              { TKLTH }
     leq                              { TKLEQ }
     gth                              { TKGTH }
     geq                              { TKGEQ }
     false                            { TKFALSE }
     true                             { TKTRUE }
     skip                             { TKSKIP }
     becomes                          { TKBECOMES }
     if                               { TKIF }
     then                             { TKTHEN }
     else                             { TKELSE }
     fi                               { TKFI }
     while                            { TKWHILE }
     inv                              { TKINV }
     do                               { TKDO }
     od                               { TKOD }
     all                              { TKALL }
     ex                               { TKEX }
     vars                             { TKVARS }
     id                             { TKID $$ }
     int                            { TKINT $$ }


%right imp
%left or
%left and
%nonassoc not
%nonassoc equ neq lth leq gth geq
%left '+' '-'                                                                               
%left '*' '/' '%'
%%

Program:
    vars Decl_list '{' Exp '}' Cmd '{' Exp '}'
    { Program {vars = $2, pro = $4, cmd = $6, post = $8} }

Decl_list: {- empty -} { [] }
    | Decl { [$1] }
    | Decl_list Decl { $2:$1 }

Decl: id { SIMPLEVAR $1 }
    | id '[' ']' { ARRAYVAR $1 }

Cmd : skip { SKIP }
    | id becomes Exp { ASSIGN (ID $1) $3 }
    | id '[' Exp ']' becomes Exp { UPDATE (ID $1) $3 $6 }
    | if Exp then Cmd else Cmd fi { IF $2 $4 $6 }
    | while Exp inv Exp do Cmd od { WHILE $2 $4 $6 }
    | Cmd ';' Cmd { SEQ $1 $3 }

Exp : true  { TRUE }
    | false { FALSE }
    | int { INT $1 }
    | id { ID $1 }
    | id '[' Exp ']' { SELECT (ID $1) $3 }
    | id '[' Exp becomes Exp ']' { STORE (ID $1) $3 $5 }
    | Exp '+' Exp { PLUS $1 $3 }
    | Exp '-' Exp { MINUS $1 $3 }
    | Exp '*' Exp { MUL $1 $3 }
    | Exp '/' Exp { DIV $1 $3 }
    | Exp '%' Exp { MOD $1 $3 }
    | Exp equ Exp { EQU $1 $3 }
    | Exp neq Exp { NOT (EQU $1 $3) }
    | Exp lth Exp { LTH $1 $3 }
    | Exp leq Exp { LEQ $1 $3 }
    | Exp gth Exp { GTH $1 $3 }
    | Exp geq Exp { GEQ $1 $3 }
    | not Exp { NOT $2 }
    | Exp and Exp { AND $1 $3 }
    | Exp or Exp { OR $1 $3 }
    | Exp imp Exp { IMP $1 $3 }
    | all id '.' Exp { FORALL (ID $2) $4 }
    | ex id '.' Exp { EXISTS (ID $2) $4 } 
    | '-' Exp { MINUS (INT 0) $2 }
    | '(' Exp ')' { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

}
