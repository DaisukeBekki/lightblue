.SUFFIXES:	.hs .hi .o .dic

# Juman.dic:	ParseMorita.hs 

all:  Parser/Japanese/Juman.dic
	ghc --make -O2 -package text-1.2.1.1 -package containers-0.4.2.1 ParseText.hs

install: Parser/Japanese/Juman.dic
	cabal configure
	cabal build
	cabal install

test: Parser/Japanese/Juman.dic
	ghc --make -O2 -package text-1.2.1.1 -package containers-0.4.2.1 ParseCorpus.hs

coq: Parser/Japanese/Juman.dic
	ghc --make -O2 -package text-1.2.1.1 -package containers-0.4.2.1 DTStoProlog.hs

treebank: Parser/Japanese/Juman.dic
	ghc --make -O2 -package text-1.2.1.1 -package containers-0.4.2.1 TreebankBuilder.hs

js: Parser/Japanese/Juman.dic
	ghc --make -O2 -package text-1.2.1.1 -package containers-0.4.2.1 ParseJSeM.hs

run: ParseCorpus
	./ParseCorpus ../JSeM_beta/test.txt

haddock:
	cabal configure
	cabal haddock --hyperlink-source --html-location='http://hackage.haskell.org/package/lightblue/docs' --contents-location='http://hackage.haskell.org/package/lightblue'