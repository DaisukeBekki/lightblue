# README
## What is this repository for?

* *lightblue* is a Japanese CCG parser with DTS representations
* Current version: 0.2.2.0
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

### Prerequisite 2: JUMAN++
The followint tools must be installed before executing *lightblue*.

[JUMAN (a User-Extensible Morphological Analyzer for Japanese)](http://nlp.ist.i.kyoto-u.ac.jp/EN/index.php?JUMAN) (>= version 7.0)

[//]: # (1. blas and lapack )
[//]: # (`sudo apt-get install libblas-dev liblapack-dev` )

### Download lightblue
Do the following in the directory under which you'd like to install *lightblue*.
```
$ git clone --depth=1 https://github.com/DaisukeBekki/lightblue.git
```
This operation will create, under the current directory, a new directory *lightblue*.  Henceforth we will refer to the full path to this directory as &lt;lightblue&gt;.

### Configuration and Installation
You need to add the environment variable LIGHTBLUE and set its value as &lt;lightblue&gt;.  You may add the line `export LIGHTBLUE=<lightblue>` to .bashrc, .bash.profile, .bash_profile, or whatever configuration file for your shell.  Then move to &lt;lightblue&gt; and do the following:
```
$ cd <lightblue>
$ stack build
```

### How to run
Set the permission of the shell scripts `lightblue` to executable.
```
$ chmod 755 lightblue
```

To parse a Japanese sentence and get a text|HTML|TeX|XML representation, execute:
```
$ echo 太郎がパンを食べた。 | ./lightblue parse -s {text|html|tex|xml}
```

With `-n|--nbest` option, *lightblue* will show the N-best parse results.

With `--time` option, *lightblue* will show the execution time for parsing.

*lightblue* can be used as a part-of-speech tagger when the `-o postag` option is specified:
```
$ echo 太郎がパンを食べた。 | ./lightblue parse -o postag
```

The following command shows the list of lexical items prepared for pasing the given sentence:
```
$ echo 太郎がパンを食べた。| ./lightblue parse -o numeration
```

If you have a text file (one sentence per line) &lt;corpusfile&gt;, then you can feed it to *lightblue* by:
```
$ ./lightblue demo -f <corpusfile>
```

To parse a JSeM file and execute inferences therein, then you can feed it to *lightblue* by:
```
$ ./lightblue infer -i jsem -f <jsemfile>
```

To check the inference relations &lt;premise_1&gt;, ..., &lt;premise_n&gt; |- &lt;hypothesis&gt;, simply execute:
```
$ ./lightblue infer -f <filename>
```
where &lt;filename&gt; is the path of a text file, consisting of premises and a hypothesis with one sentence per each line:
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
$ sudo apt install haskell-mode
```

The following command creates an HTML document at: `<lightblue>/haddock/doc/html/lightblue/index.html`

```
$ stack build --haddock
```

## Contact ##

* Repo owner: [Daisuke Bekki](https://daisukebekki.github.io/)
