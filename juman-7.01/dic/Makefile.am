
MAKEINT = ../makeint/makeint
MAKEMAT = ../makemat/makemat
MAKEPAT = ../makepat/makepat
DICSORT = ../makepat/dicsort

GRM_FILES = JUMAN.grammar JUMAN.kankei JUMAN.katuyou
TBL_FILES = jumandic.mat jumandic.tab
INT_FILES := $(patsubst %.dic,%.int,$(wildcard *.dic))

dicdir = $(pkgdatadir)/dic
nodist_dic_DATA = JUMAN.connect $(TBL_FILES) jumandic.dat jumandic.pat
dist_dic_DATA = $(GRM_FILES)
EXTRA_DIST = JUMAN.connect.c *.dic

.dic.int: $(MAKEINT)
	$(MAKEINT) $<

clean:
	rm -f jumandic.* *.int JUMAN.connect

JUMAN.connect: JUMAN.connect.c
	@CPP@ -P $< | LANG=C sed "s/\(\#pragma\)/\;\1/" > $@

jumandic.tab: $(GRM_FILES) JUMAN.connect $(MAKEMAT)
	$(MAKEMAT)

jumandic.mat: jumandic.tab

$(INT_FILES): $(GRM_FILES) $(TBL_FILES) $(MAKEINT)

jumandic.dat: $(INT_FILES) $(DICSORT)
	cat $(INT_FILES) > jumandic.txt
	$(DICSORT) jumandic.txt > $@
	rm -f jumandic.txt

jumandic.pat: jumandic.dat $(MAKEPAT)
	$(MAKEPAT)
