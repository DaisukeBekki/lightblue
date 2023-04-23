# README
## What is this repository for?

* *lightblue* is a Japanese CCG parser with DTS representations
* Current version: 0.2.0.0
* Copyright owner: Daisuke Bekki


## Installing lightblue
### Prerequisite 1: Haskell Stack
In Linux:
```
$ wget -qO- https://get.haskellstack.org/ | sh
```
In Mac:
```
$ brew install haskell-stack
```
See https://docs/haskellstack.org/en/stable/README/#how-to-install for details.

### Prerequisite 2: command-line tools
The followint tools must be installed before executing *lightblue*.

  1. [JUMAN (a User-Extensible Morphological Analyzer for Japanese)](http://nlp.ist.i.kyoto-u.ac.jp/EN/index.php?JUMAN) (>= version 7.0)
  1. tidy (only for prettyprinting XML outputs)
In Linux:
```
$ sudo apt install tidy
```
In Mac:
```
$ sudo port install tidy
```

[//]: # (1. blas and lapack )
[//]: # (`sudo apt-get install libblas-dev liblapack-dev` )

### Download lightblue
Do the following in the directory under which you'd like to install *lightblue*.
```
$ git clone https://github.com/DaisukeBekki/lightblue.git
```
This operation will create the directory *lightblue* (henceforth we will refer to this directory as <lightblue>) under the current directory.

### Configuration and Installation
You need to add the environment variable LIGHTBLUE and set its value as <lightblue>.  You may add the line `export LIGHTBLUE=<lightblue>` to .bashrc, .bash.profile, .bash_profile, or whatever configuration file for your shell.  Then move to <lightblue> and:
```
$ cd <lightblue>
$ stack setup
$ stack build
```

### How to run
Set the permission of two shell scripts `lightblue` and `tidy` to executable.
```
$ chmod 755 lightblue
$ chmod 755 tidy
```

To parse a Japanese sentence and get a text|HTML|TeX|XML representation, execute:
```
$ echo 太郎がパンを食べた。 | ./lightblue parse -s {text|html|tex}
```
or
```
$ echo 太郎がパンを食べた。 | ./lightblue parse -s xml | ./tidy
```

With '-n|--nbest' option, *lightblue* will show the N-best parse results.

With '--time' option, *lightblue* will show the execution time for parsing.

*lightblue* can be used as a part-of-speech tagger when the `-o postag` option is specified:
```
$ echo 太郎がパンを食べた。 | ./lightblue parse -o postag
```

The following command shows the list of lexical items prepared for pasing the given sentence:
```
$ echo 太郎がパンを食べた。| ./lightblue parse -o numeration
```

If you have a text file (one sentence per line) <corpusfile>, then you can feed it to *lightblue* by:
```
$ ./lightblue demo -f <corpusfile>
```

To parse a JSeM file and execute inferences therein, then you can feed it to *lightblue* by:
```
$ ./lightblue infer -i jsem -f <jsemfile>
```

To check the inference relations <premise_1>, ..., <premise_n> |- <hypothesis>, simply execute:
```
$ ./lightblue infer -f <filename>
```
where <filename> is the path of a text file, consisting of premises and a hypothesis with one sentence per each line:
```
<premise_1>
...
<premise_n>
<hypothesis>
```

Check also:
```
$ lightblue --help
$ lightblue --version
$ lightblue --stat
```

### For developpers ###
Installing Haskell-mode for Emacs will help.
```
$ sudo apt-get install haskell-mode
```

The following command creates an HTML document at: `<lightblue>/haddock/doc/html/lightblue/index.html`

```
$ stack build --haddock
```

## Contact ##

* Repo owner: [Daisuke Bekki](http://www.is.ocha.ac.jp/~bekki/)
* [Learn Markdown](https://bitbucket.org/tutorials/markdowndemo)
