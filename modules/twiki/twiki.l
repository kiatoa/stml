;; twiki

period           \.
comma            ,
semicolon        \;
opensq           \[
closesq          \]
opensquig        \{
closesquig       \}

digit            [0-9]
letter           [a-zA-Z]
okchars          [_%\/\.:\*\+\-\(\)\\#']
escape           \\ 
whitespace       [ \9]
linefeed         \n
bang             !
plaintext        ({letter}|{digit}|{okchars}|{whitespace}|{linefeed})+

%%

{opensq}                        (list 'opensq     yytext)
{closesq}                       (list 'closesq    yytext)

{opensquig}                     (list 'opensquig  yytext)
{closesquig}                    (list 'closesquig yytext)

{bang}                          (list 'bang       yytext)
{plaintext}                     (list 'plaintext  yytext)

<<EOF>>                         (list 'end-of-input #f ) ;; yyline)

<<ERROR>>                       (lex-error (conc yyline " : illegal character ") (yygetc))
