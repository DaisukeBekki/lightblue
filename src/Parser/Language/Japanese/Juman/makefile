.SUFFIXES:	.hs .hi .o .dic

### Programs ###
GHC=	ghc
MORITA=	ParseMorita
HAYASHIBE=	ParseHayashibe

### Data ###
JUMANDIR=	../../Juman
JUMANDIC=	$(wildcard $(abspath $(JUMANDIR)/*.dic))

### Targets ###
all:	Juman.dic

### Dependencies ###
Juman.dic:	ParseMorita $(JUMANDIC) KawaharaFrame.txt
	./ParseMorita > Juman.dic.bak
	cp Juman.dic.bak ../Juman.dic
KawaharaFrame.txt:	ParseHayashibe
	./ParseHayashibe > KawaharaFrame.txt.bak
	cp KawaharaFrame.txt.bak KawaharaFrame.txt
$(MORITA):	$(MORITA).hs
	ghc -package text-1.2.1.1 $(MORITA).hs
$(HAYASHIBE):	$(HAYASHIBE).hs
	ghc -package text-1.2.1.1 $(HAYASHIBE).hs
