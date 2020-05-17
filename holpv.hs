module Main (main) where
import System.IO
import System.Process
import System.Exit
import Data.List
import HOLCommon
import HOLLexer
import HOLParser
import Text.Printf

str_expr :: Exp -> [Char]
str_expr FALSE          = do printf "false"
str_expr TRUE           = do printf "true"
str_expr (INT x)        = do printf "%d" x
str_expr (ID x)         = do printf "%s" x
str_expr (SELECT x y)   = do printf "(select %s %s)" (str_expr x) (str_expr y)
str_expr (STORE x y z)  = do printf "(store %s %s %s)" (str_expr x) (str_expr y) (str_expr z)
str_expr (PLUS x y)     = do printf "(+ %s %s)" (str_expr x) (str_expr y)
str_expr (MINUS x y)    = do printf "(- %s %s)" (str_expr x) (str_expr y)
str_expr (MUL x y)      = do printf "(* %s %s)" (str_expr x) (str_expr y)
str_expr (DIV x y)      = do printf "(div %s %s)" (str_expr x) (str_expr y)
str_expr (MOD x y)      = do printf "(mod %s %s)" (str_expr x) (str_expr y)
str_expr (EQU x y)      = do printf "(= %s %s)" (str_expr x) (str_expr y)
str_expr (LTH x y)      = do printf "(< %s %s)" (str_expr x) (str_expr y)
str_expr (LEQ x y)      = do printf "(<= %s %s)" (str_expr x) (str_expr y)
str_expr (GTH x y)      = do printf "(> %s %s)" (str_expr x) (str_expr y)
str_expr (GEQ x y)      = do printf "(>= %s %s)" (str_expr x) (str_expr y)
str_expr (NOT x)        = do printf "(not %s)" (str_expr x)
str_expr (AND x y)      = do printf "(and %s %s)" (str_expr x) (str_expr y)
str_expr (OR x y)       = do printf "(or %s %s)" (str_expr x) (str_expr y)
str_expr (IMP x y)      = do printf "(=> %s %s)" (str_expr x) (str_expr y)
str_expr (FORALL x y)   = do printf "(forall ((%s Int)) %s)" (str_expr x) (str_expr y)
str_expr (EXISTS x y)   = do printf "(exists ((%s Int)) %s)" (str_expr x) (str_expr y)
-- str_expr _ = "" -- redundant

subst :: Exp -> Exp -> Exp -> Exp
subst v e FALSE         = FALSE
subst v e TRUE          = TRUE
subst v e (INT x)       = (INT x)
subst v e (ID x)        = if (ID x) == v then e else (ID x)
subst v e (SELECT x y)  = SELECT (subst v e x) (subst v e y)
subst v e (STORE x y z) = STORE (subst v e x) (subst v e y) (subst v e z)
subst v e (PLUS x y)    = PLUS (subst v e x) (subst v e y)
subst v e (MINUS x y)   = MINUS (subst v e x) (subst v e y)
subst v e (MUL x y)     = MUL (subst v e x) (subst v e y)
subst v e (DIV x y)     = DIV (subst v e x) (subst v e y)
subst v e (MOD x y)     = MOD (subst v e x) (subst v e y)
subst v e (EQU x y)     = EQU (subst v e x) (subst v e y)
subst v e (LTH x y)     = LTH (subst v e x) (subst v e y)
subst v e (LEQ x y)     = LEQ (subst v e x) (subst v e y)
subst v e (GTH x y)     = GTH (subst v e x) (subst v e y) 
subst v e (GEQ x y)     = GEQ (subst v e x) (subst v e y)
subst v e (NOT x)       = NOT (subst v e x)
subst v e (AND x y)     = AND (subst v e x) (subst v e y)
subst v e (OR x y)      = OR (subst v e x) (subst v e y) 
subst v e (IMP x y)     = IMP (subst v e x) (subst v e y) 
subst v e (FORALL x y)  = if x == v then (FORALL x y) else FORALL x (subst v e y)
subst v e (EXISTS x y)  = if x == v then (EXISTS x y) else EXISTS x (subst v e y)

wp :: [Exp] -> Cmd -> Exp -> ([Exp], Exp) 
wp acc SKIP post            = (acc, post)
wp acc (ASSIGN x e) post    = (acc, subst x e post)
wp acc (UPDATE v i e) post  = (acc, subst v (STORE v i e) post) 
wp acc (IF b c1 c2) post    = -- nazo
    (acc2, AND (IMP b pre1) (IMP (NOT b) pre2))
    where
        (acc1, pre1) = wp acc c1 post
        (acc2, pre2) = wp acc1 c2 post
wp acc (WHILE b inv c) post =
    (p:q:acc_new, inv)
    where
        (acc_new, post_new) = wp acc c inv
        p = IMP (AND inv b) post_new
        q = IMP (AND inv (NOT b)) post
wp acc (SEQ c1 c2) post     =
    wp acc_new c1 post_new
    where
        (acc_new, post_new) = wp acc c2 post

print_string expr = do
    putStrLn (str_expr expr)

gen_vars h [] = do hPutStr h ""
gen_vars h ((SIMPLEVAR v):vs) = do
    hPutStr h "(declare-const "
    hPutStr h v
    hPutStrLn h " Int)"
    gen_vars h vs
gen_vars h ((ARRAYVAR v):vs) = do
    hPutStr h "(declare-const "
    hPutStr h v
    hPutStrLn h " (Array Int Int))"
    gen_vars h vs

generate vars [] = do putStr "" 
generate vars (p:ps) = do
    handle <- openFile "smt.in" WriteMode
    hPutStrLn handle "(set-logic ALL)"
    gen_vars handle vars
    hPutStr handle "(assert "
    hPutStr handle (str_expr (NOT p))
    hPutStrLn handle ")"
    hPutStrLn handle "(check-sat)"
    --hPutStrLn handle "(apply ctx-solver-simplify)"
    hPutStrLn handle "(get-model)"
    hClose handle
    system "z3 -smt2 smt.in > smt.out"
    handle <- openFile "smt.out" ReadMode
    contents <- hGetContents handle
    let contents_list = lines contents
    --print contents_list
    if isInfixOf "unsat" (head contents_list)
        then
            putStrLn (printf "Success: %s" (str_expr p))
        else
            putStr (printf "Failure: %s\n%s" (str_expr p) contents)
            --sequence_ putStrLn contents_list
    hClose handle
    generate vars ps

main = do
    s <- getContents
    let token = alexScanTokens s
    --print token
    let ptree = holparser token
    --print ptree 
    putStr "pro: "
    print_string (pro ptree)
    putStr "post: "
    print_string (post ptree)
    let (acc, weakest) = (wp [] (cmd ptree) (post ptree))
    putStr "weakest: "
    print_string weakest
    let prop_list = (IMP (pro ptree) weakest):acc
    generate (vars ptree) prop_list
