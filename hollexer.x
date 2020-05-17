{
module HOLLexer where
import HOLCommon
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+               ;
  "--".*                ;
  ";"                   { \s -> TKSEMICOLON }
  "."                   { \s -> TKPERIOD }
  "("                   { \s -> TKLPAR }
  ")"                   { \s -> TKRPAR }
  "{"                   { \s -> TKLCUR }
  "}"                   { \s -> TKRCUR }
  "["                   { \s -> TKLBRA }
  "]"                   { \s -> TKRBRA }
  "+"                   { \s -> TKPLUS }
  "-"                   { \s -> TKMINUS }
  "*"                   { \s -> TKASTERISK }
  "div"                 { \s -> TKDIV }
  "mod"                 { \s -> TKMOD }
  "not"                 { \s -> TKNOT }
  "&"                   { \s -> TKAND }
  "|"                   { \s -> TKOR }
  "=>"                  { \s -> TKIMP }
  "="                   { \s -> TKEQU }
  "!="                  { \s -> TKNEQ }
  "<"                   { \s -> TKLTH }
  "<="                  { \s -> TKLEQ }
  ">"                   { \s -> TKGTH }
  ">="                  { \s -> TKGEQ }
  "false"               { \s -> TKFALSE }
  "true"                { \s -> TKTRUE }
  "skip"                { \s -> TKSKIP }
  ":="                  { \s -> TKBECOMES }
  "if"                  { \s -> TKIF }
  "then"                { \s -> TKTHEN }
  "else"                { \s -> TKELSE }
  "fi"                  { \s -> TKFI }
  "while"               { \s -> TKWHILE }
  "inv"                 { \s -> TKINV }
  "do"                  { \s -> TKDO }
  "od"                  { \s -> TKOD }
  "ALL"                 { \s -> TKALL }
  "EX"                  { \s -> TKEX }
  "vars"                { \s -> TKVARS }
  $digit+               { \s -> TKINT (read s) }
  $alpha [$alpha $digit \_ \']*     { \s -> TKID s }

{
-- Each action has type :: String -> Token

--main = do
--  s <- getContents
--  print (alexScanTokens s)
}
