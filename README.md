# README
## What is this repository for?

* *lightblue* is a Japanese CCG parser with DTS representations.
* The current version is 0.1.1.0.

## How do I get set up?

### Prerequisite: Haskell Platform
  1. GHC/cabal (>= version 7.0)
    * Debian: `sudo apt-get install haskell-platform` `sudo apt-get install haskell-mode`
    * Mac: [Haskell for Mac OS X](https://ghcformacosx.github.io/)

After installing GHC, update cabal.
```
#!shell
$ cabal update
```

### Prerequisite: command-line tools
  1. juman (>= version 5.0)
    * Debian: `sudo apt-get install juman`
    * Mac: [日本語形態素解析システム JUMAN](http://nlp.ist.i.kyoto-u.ac.jp/index.php?JUMAN)
  1. nkf (only for handling EUC-code for juman)
    * Debian: `sudo apt-get install nkf`
    * Mac: `sudo port install nkf`
  1. tidy (only for prettyprinting XML outputs)
    * Debian: `sudo apt-get install tidy`
    * Mac: `sudo port install tidy`

### Download
Do the following in the directory under which you'd like to install *lightblue*.
```
#!shell
$ git clone git@bitbucket.org:DaisukeBekki/lightblue.git
```
This operation will create the directory *lightblue* (we will call this directory as <lightblue> in the following document) under the directory in which you did the above.

### Configuration
First you have to rewrite the value of `jumandicpath` in `<lightblue>/Parser/Japanese/Lexicon.hs` to the location of `<lightblue>/Parser/Japanese/Juman.dic` according to your environment.

Then move to <lightblue>, create a sandbox environment there, and check the dependencies as follows.
```
#!shell
$ cd <lightblue>
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build
```

If this fails, please install the following libraries manually:
```
$ cabal install containers-0.4.2.1`
$ cabal install text-1.2.1.1`
```

### Generating the Document
The HTML document is created by the following command in <lightblue>:
```
#!shell
$ cabal haddock
```
The generated document is found at: `<lightblue>/dist/doc/html/lightblue/index.html`

### Installation
If the build is successful, then you can install *lightblue-0.1.1.0* in your GHC system.
```
#!shell
$ cabal install
```
If succeeded, executable is found at `<home>/.cabal/bin/lightblue` and `<home>/.cabal/bin/lightbluetest`.
You may want to set a path to `<home>/.cabal/bin`.

### How to run tests
To parse a Japanese sentence and get a text representation, execute:
```
#!shell
$ echo 太郎がパンを食べた。 | lightblue -text
```
or equivalently,
```
#!shell
$ ./parse 太郎がパンを食べた。
```

If you want an XML output, do
```
#!shell
$ echo 太郎がパンを食べた。 | lightblue -xml
```
or equivalently,
```
#!shell
$ ./parse2xml 太郎がパンを食べた。
```

*lightblue* can be used as a part-of-speech tagger with the `-postag` option:
```
#!shell
$ echo 太郎がパンを食べた。 | lightblue -postag
```

If you have a text file (one sentence per line) <corpus>, then you can feed it to *lightblue* by:
```
#!shell
$ lightbluetest <corpus>
```

### Deployment instructions

## Contribution guidelines ###

* Writing tests
* Code review
* Other guidelines

## Who do I talk to? ###

* Repo owner or admin
* Other community or team contact
* [Learn Markdown](https://bitbucket.org/tutorials/markdowndemo)