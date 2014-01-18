all : manual.pdf web-page.html

manual.pdf : manual.txt
	a2x -a toc -f pdf manual.txt
	# asciidoc -a toc plan.txt
	a2x -f chunked -a toc manual.txt

