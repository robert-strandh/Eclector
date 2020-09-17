NAME=eclector

SOURCES=$(NAME).texi $(wildcard chap-*.texi)

FIGURES=$(wildcard *.png)

.PHONY: all clean

all : $(NAME).info $(NAME).pdf $(NAME).html

$(NAME).info: $(SOURCES) $(FIGURES)
	makeinfo --output $@ $(NAME).texi

$(NAME).pdf: $(SOURCES) $(FIGURES)
	texi2pdf $(NAME).texi

$(NAME).html: $(SOURCES) $(FIGURES) $(wildcard *.css)
	makeinfo --html --no-split              \
	         --css-include=style-syntax.css \
	         --css-include=style-common.css \
	         --css-include=style-single.css \
	         $(NAME).texi
	sbcl --noinform --disable-debugger --load highlight-listings.lisp --quit

clean:
	rm -f *.cp *.cps *.fn *.fns \
	      *.aux *.toc *.log \
	      $(NAME).info $(NAME).pdf $(NAME).html
