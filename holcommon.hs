module HOLCommon where

-- The token type:
data Token =
    TKLET         |
    TKSEMICOLON   |
    TKPERIOD      |
    TKLPAR        |
    TKRPAR        |
    TKLBRA        |
    TKRBRA        |
    TKLCUR        |
    TKRCUR        |
    TKPLUS        |
    TKMINUS       |
    TKASTERISK    |
    TKDIV         |
    TKMOD         |
    TKNOT         |
    TKAND         |
    TKOR          |
    TKIMP         |
    TKEQU         |
    TKNEQ         |
    TKLTH         |
    TKLEQ         |
    TKGTH         |
    TKGEQ         |
    TKFALSE       |
    TKTRUE        |
    TKSKIP        |
    TKBECOMES     |
    TKIF          |
    TKTHEN        |
    TKELSE        |
    TKFI          |
    TKWHILE       |
    TKINV         |
    TKDO          |
    TKOD          |
    TKALL         |
    TKEX          |
    TKVARS        |
    TKID String   |
    TKINT Int
    deriving (Eq,Show)


data Exp
    = TRUE
    | FALSE
    | INT Int
    | ID String
    | SELECT Exp Exp
    | STORE Exp Exp Exp
    | PLUS Exp Exp
    | MINUS Exp Exp
    | MUL Exp Exp
    | DIV Exp Exp
    | MOD Exp Exp
    | EQU Exp Exp
    | LTH Exp Exp
    | LEQ Exp Exp
    | GTH Exp Exp
    | GEQ Exp Exp
    | NOT Exp
    | AND Exp Exp
    | OR Exp Exp
    | IMP Exp Exp
    | FORALL Exp Exp
    | EXISTS Exp Exp
    deriving (Eq, Show)

data Cmd
    = SKIP
    | ASSIGN Exp Exp
    | UPDATE Exp Exp Exp
    | IF Exp Cmd Cmd
    | WHILE Exp Exp Cmd
    | SEQ Cmd Cmd
    deriving (Eq, Show)

data Decl
    = SIMPLEVAR String
    | ARRAYVAR String
    deriving (Eq, Show)

--data Decl_list
--    = Decl_list [Decl]
--    deriving (Show)

data Program
    = Program {
        vars :: [Decl],
        pro  :: Exp,
        cmd  :: Cmd,
        post :: Exp
    }
    deriving (Show)

