
twiki.l.scm : twiki.l
	csi -batch -eval '(use silex)(if (lex "twiki.l" "twiki.l.scm")(exit 0)(exit 1))'

test-silex : twiki.l.scm test-silex.scm
	csc test-silex.scm

twikicount : twiki.l.scm twikicount.scm
	csc twikicount.scm

