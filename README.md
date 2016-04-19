# README
## What is this repository for?

* *lightblue* is a Japanese CCG parser with DTS representations.  
* Current version: 0.1.1.0.  
* Copyright owner: Daisuke Bekki


## How do I get set up?

### Prerequisite: Haskell Platform
  1. GHC (>= version 7.4)/Cabal (>= version 1.20)
    * Linux: [The Glasgow Haskell Compiler](https://www.haskell.org/ghc/) 
    * Mac: [Haskell for Mac OS X](https://ghcformacosx.github.io/)

In Debian, you may just do it by `sudo apt-get install haskell-platform`.

After installing GHC, update cabal.
```
#!shell
$ cabal update
```

Installing Haskell-mode for Emacs will help.
```
#!shell
$ sudo apt-get install haskell-mode
```

### Prerequisite: command-line tools
The followint tools must be installed before executing *lightblue*.

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
This operation will create the directory *lightblue* (henceforth we will refer to this directory as <lightblue>) under the directory in which you did the above.

### Configuration
First you need to add the environment variable LIGHTBLUE and set its value as <lightblue>.  Suppose that <lightblue> is `/home/lightblue/`, you are supposed to add the following line to .bashrc (or whatever configuration file for your shell).
`export LIGHTBLUE=/home/lightblue/`

Then move to <lightblue>, create a sandbox environment there, and check the dependencies as follows.
```
#!shell
$ cd <lightblue>
$ cabal sandbox init
$ cabal install --only-dependencies
```
If everything is ok, then build *lightblue* there.

```
$ cabal build
```

### Installation
If the build is successful, then you may install *lightblue-0.1.1.0* in the sandbox.
```
#!shell
$ cabal install
```
If succeeded, executable is found at `<lightblue>/.cabal-sandbox/bin/lightblue` and `<lightblue>/.cabal-sandbox/bin/lightbluetest`.  You may want to add a path to `<lightblue>/.cabal-sandbox/bin`.

### Generating the Document
The HTML document is created by the following command in <lightblue>:
```
#!shell
$ cabal haddock
```
The generated document is found at: `<lightblue>/dist/doc/html/lightblue/index.html`

### How to run tests
Set the permission of scripts `parse` and `parse2xml` to executable.
```
#!shell
$ chmod 755 parse
$ chmod 755 parse2xml
```

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

## Contact ##

* Repo owner: [Daisuke Bekki](http://www.is.ocha.ac.jp/~bekki/)
* [Learn Markdown](https://bitbucket.org/tutorials/markdowndemo)