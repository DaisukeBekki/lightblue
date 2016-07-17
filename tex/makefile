.SUFFIXES:	.tex .dvi .ps .pdf .aux .log .toc .lof .lot .bbl .bib .blg

### プログラム ###
TEX=		platex -kanji=utf8
BIBTEX=		jbibtex -min-crossrefs=0 
DVIOUT=		/usr/share/dviout/dviout.exe
DVIPS=		dvipsk
RM=			rm -f

### データ ###
SOURCEDIR=	.
NAME=		index
#NAME=	bccwj_homelos
#NAME=	jobs

### ターゲット ###
all:		$(NAME).dvi
dvi:		$(NAME).dvi
bbl:		$(NAME).bbl
ps:			$(NAME).ps
pdf:		$(NAME).pdf
view:		$(NAME).dvi
			$(DVIOUT) $< &

### 依存関係 ###
$(NAME).dvi:	$(NAME).tex # outjis.txt
$(NAME).ps:		$(NAME).dvi
$(NAME).pdf:	$(NAME).ps
$(NAME).aux:	$(NAME).log
#outjis.txt:		out.txt
#			nkf -W8 -s out.txt > outjis.txt

### サフィクス・ルール ###
.tex.dvi:
			$(TEX) $<
			(while egrep '^No file $*.lo' $*.log; \
				do	$(TEX) $<; \
				done)
			(while egrep 'Warning: There were undefined citations.' $*.log; \
				do	$(BIBTEX) $*; \
					$(TEX) $<; \
				done)
			(while egrep 'Citation\(s\) may have changed' $*.log; \
				do	$(TEX) $<; \
				done)
			(while egrep '^LaTeX Warning: Label\(s\) may have changed.' $*.log; \
				do	$(TEX) $<; \
				done)
			(if egrep '^LaTeX Warning: There were undefined references.' $*.log; \
				then	$(TEX) $<; \
				fi)
			(if egrep '^LaTeX Warning: There were undefined references.' $*.log; \
				then	cat $(NAME).log | grep "undefined on"; \
				fi)
.tex.aux:
			$(TEX) $<
.bib.bbl:
			$(BIBTEX) $*
.dvi.ps:
			$(DVIPS) -D1200 -t a4 -Ppdf -G0 -f -z $^ | bkmk2uni > $@
.ps.pdf:
			start $^
do:
#			nkf -W8 -s out.txt > outjis.txt
			$(TEX) $(NAME).tex
clean:
			$(RM) *~
			$(RM) $(NAME).{aux,log,nav,out,dvi,pdf,ps,snm,tag,toc}
