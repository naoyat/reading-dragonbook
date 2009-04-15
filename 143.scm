%{
  #| definitions of manifest constants
     LT LE EQ NE GT GE
     IF THEN ELSE ID NUMBER RELOP |#
}%

;; regular definitions
delim    [ \t\n]
ws       {delim}+
letter   [A-Za-z]
digit    [0-9]
id       {letter}({letter}|{digit})*
number   {digit}+(\.{digit}+)?(E[+-]?{digit}+)?

%%

{ws}     { #| no action and no return |# }
if       { IF }
then     { THEN }
else     { ELSE }
{id}     { (set-yylval! (installID)) ID }
{number} { (set-yylval! (installNum)) NUMBER }
"<"      { (set-yylval! LT) RELOP }
"<="     { (set-yylval! LE) RELOP }
"="      { (set-yylval! EQ) RELOP }
"<>"     { (set-yylval! NE) RELOP }
">"      { (set-yylval! GT) RELOP }
">="     { (set-yylval! GE) RELOP }

%%

(define (installID)
  "function to install the lexeme, whose first character is pointed to by yytext,
   and whose length is yyleng, into the symbol table and return a pointer thereto"
  )
(define (installNum)
  "similar to installID, but puts numerical constants into a separate table"
  )
