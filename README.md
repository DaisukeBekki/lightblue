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

### Prerequisite 2: Installation of morphological analyzer (KWJA or Juman/Jumanpp)
Either of the following tools must be installed before executing *lightblue*.

- [KWJA (Japanese text analyzer)](https://github.com/ku-nlp/kwja)

- [JUMAN (a User-Extensible Morphological Analyzer for Japanese)](http://nlp.ist.i.kyoto-u.ac.jp/EN/index.php?JUMAN) (>= version 7.0) 

- [JUMAN++](https://nlp.ist.i.kyoto-u.ac.jp/?JUMAN%2B%2B) 

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

Set the permission of the shell scripts `lightblue` to executable.
```
$ chmod 755 lightblue
```

## Running lightblue
### Quick Start

To parse a Japanese sentence and get a parsing result in a text format, execute:
```
$ echo 太郎がパンを食べた。 | ./lightblue parse -s text
```

To see a parsing result in HTML formal, execute (choose your browser):
```
$ echo 太郎がパンを食べた。 | ./lightblue parse -s html > result.html; firefox result.html
```

If you have a text file (one sentence per line) &lt;corpusfile&gt;, then you can feed it to *lightblue* by:
```
$ ./lightblue parse -s html -f <corpusfile>
```

To parse a JSeM file and execute inferences therein, then you can feed it to *lightblue* by:
```
$ ./lightblue jsem -f <jsemfile>
```

### Usage
The syntax of the lightblue command is as follows:
```
stack run lightblue -- <command> <local options> <global options>
```

|Command         |                                                                      |
|:---------------|:---------------------------------------------------------------------|
|```parse```     |Parse Japanese sentences and get a parsing result.                    |
|```jsem```      |Parse a JSeM file and execute inferences therein.                     |
|```numeration```|Shows the list of lexical items prepared for pasing the given sentence|
|```version```   |Print the lightblue version.                                          |
|```stat```      |Print the lightblue statistics.                                       |

Each of ```parse ``` and ```jsem``` commands has a set of local options.

|Local Option for ```parse```                     |Default   |Description                                                    |  
|:------------------------------------------------|:---------|:--------------------------------------------------------------|
|```-o``` or ```--output {tree\|postag}```        |```tree```|Specify the output content.<br>
                                                              ```tree```: Shows parse trees and their type check results.<br> 
                                                              ```postag```: Use lightblue as a part-of-speech tagger         |
|```-p``` or ```--prover {Wani\|Null}```          |```Wani```|Choose prover.<br>
                                                              ```Wani```: Use the Wani prover (Daido and Bekki 2020)<br>     |
                                                              ```None```: Use the null prover (that returns no diagrams).    |

|Local Option for ```jsem```                      |Default   |Description                           |  
|:------------------------------------------------|:---------|:-------------------------------------|
|```-s``` or ```--style {text\|tex\|xml\|html}``` |```text```|Print results in the specified format |
|```-p``` or ```--prover {Wani\|Null}```          |```Wani```|Choose prover                         |
|```--nsample <int>```                            |```-1```  |How many data to process              |

The global options are common to all commands.

|Global Option                                    |Default   |Description                                                     |
|:------------------------------------------------|:---------|:---------------------------------------------------------------|
|```-s``` or ```--style {text\|tex\|xml\|html}``` |```text```|Show parse results in the specified format.                     |
|```-f``` or ```--file <filepath>```              |          |Reads input texts from <filepath><br>(Specify '-' to use stdin) |
|```-m``` or ```--ma {juman\|jumanpp\|kwja}```    |```kwja```|Specify morphological analyzer (default: KWJA)                  |
|```-b``` or ```--beam <int>```                   |```32```  |Set the beam width to <int>                                     |
|```--nparse <int>```                             |```-1```  |Search only N-best parse trees for each sentence (A negative value means no filtering) |                      |
|```--ntypecheck <int>```                         |```-1```  |Search only N-best diagrams for each type checking of a logical form (A negative value means no filtering) |
|```--nproof <int>```                             |```-1```  |Search only N-best diagrams for each proof search (A negative value means no filtering) |
|```--noTypeCheck```                              |          |If True, execute no type checking for LFs.                      |
|```--noInference```                              |          |If true, execute no inference.                                  |
|```--time```                                     |          |Show the execution time in stderr.                              |
|```--verbose```                                  |          |Show logs of type inferer and type checker.                     |

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
