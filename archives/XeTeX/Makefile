# -*- encoding: utf-8-unix; -*-
# File-name:    <Makefile>
# Create:       <2011-11-09 17:09:18 ran9er>
# Time-stamp:   <2011-11-09 17:09:25 ran9er>

NAME = out
SOURCE = $(addsuffix .tex , $(NAME))
TARGET = $(addsuffix .pdf , $(NAME))
COMPILER = xelatex
MKIDX = makeindex
obj1 = $(shell ls *.tex | grep '^[0-9]'| sed 's/\(.*\)\.tex/\\include\{\1\}/g' > IcldLst)
obj2 = $(shell sed '/^%%include%%/r IcldLst' main.tex > out.tex ; rm IcldLst)
O = *.log *.aux *.idx *.ilg *.ind *.toc *.out

$(TARGET): *.tex
	$(obj1)
	$(obj2)
	$(COMPILER) $(SOURCE) $(TARGET)
	$(COMPILER) $(SOURCE) $(TARGET)
	rm out.tex


clean:
	rm -f $(O)
