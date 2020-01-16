
MAKEINT = ../makeint/makeint
MAKEMAT = ../makemat/makemat
MAKEPAT = ../makepat/makepat
DICSORT = ../makepat/dicsort

GRM_FILES = ../dic/JUMAN.grammar ../dic/JUMAN.kankei ../dic/JUMAN.katuyou
TBL_FILES = ../dic/jumandic.mat ../dic/jumandic.tab
INT_FILES := $(patsubst %.dic,%.int,$(wildcard *.dic))

dicdir = $(pkgdatadir)/autodic
nodist_dic_DATA = jumandic.dat jumandic.pat
EXTRA_DIST = Auto.dic

.dic.int: $(MAKEINT)
	$(MAKEINT) $<

clean:
	rm -f jumandic.* *.int

$(INT_FILES): $(GRM_FILES) $(TBL_FILES) $(MAKEINT)

jumandic.dat: $(INT_FILES) $(DICSORT)
	cat $(INT_FILES) > jumandic.txt
	$(DICSORT) jumandic.txt > $@
	rm -f jumandic.txt

jumandic.pat: jumandic.dat $(MAKEPAT)
	$(MAKEPAT)
