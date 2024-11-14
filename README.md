# README
## What is this repository for?

* *lightblue* is a Japanese CCG parser with DTS representations
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
One of the following Japanese morphological analyzers must be installed before executing *lightblue*.

- [KWJA](https://github.com/ku-nlp/kwja)

- [JUMAN](http://nlp.ist.i.kyoto-u.ac.jp/EN/index.php?JUMAN) (>= version 7.0) 

- [JUMAN++](https://nlp.ist.i.kyoto-u.ac.jp/?JUMAN%2B%2B) 

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

If you have a text file (one sentence per line) &lt;corpusfile&gt;, then you can feed its path to *lightblue* by:
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
./lightblue <command> <local options> <global options>
```

|Command         |                                                                       |
|:---------------|:----------------------------------------------------------------------|
|```parse```     |Parse Sentences and returns parsing results.                           |
|```jsem```      |Parse a JSeM file and execute inferences.                              |
|```numeration```|Shows the list of lexical items prepared for parsing the given sentence|
|```version```   |Print the lightblue version.                                           |
|```stat```      |Print the lightblue statistics.                                        |

Each of ```parse ``` and ```jsem``` commands has a set of local options.

|Local Options for ```parse```                     |Default   |Description                                                    |  
|:-------------------------------------------------|:---------|:--------------------------------------------------------------|
|```-o``` or ```--output {tree\|postag}```         |```tree```|Specify the output content.<br>```tree```: Outputs parse trees and their type check results.<br> ```postag```: Outputs only lexical items (Use lightblue a part-of-speech tagger) |
|```-p``` or ```--prover {Wani\|Null}```           |```Wani```|Choose a prover.<br>```Wani```: Use Wani prover (Daido and Bekki 2020)<br>```None```: Use the null prover (that always returns no diagrams).|

|Local Options for ```jsem```                      |Default   |Description                           |  
|:-------------------------------------------------|:---------|:-------------------------------------|
|```-p``` or ```--prover {Wani\|Null}```           |```Wani```|Choose a prover.<br>```Wani```: Use Wani prover (Daido and Bekki 2020)<br>```None```: Use the null prover (that always returns no diagrams).|
|```--jsemid <text>```                             |```all``` |Skip JSeM data the JSeM ID of which is not equial to this value.           |
|```--nsample <int>```                             |```-1```  |Specify a number of JSeM data to process (A negative value means all data) |

The global options are common to all commands.

|Global Options                                    |Default   |Description                                                     |
|:------------------------------------------------|:---------|:---------------------------------------------------------------|
|```-s``` or ```--style {text\|tex\|xml\|html}``` |```text```|Show results in the specified format.                     |
|```-f``` or ```--file <filepath>```              |          |Read input texts from <filepath><br>(Specify '-' to use stdin) |
|```-m``` or ```--ma {juman\|jumanpp\|kwja}```    |```kwja```|Specify morphological analyzer (default: KWJA)                  |
|```-b``` or ```--beam <int>```                   |```32```  |Set the beam width to <int>                                     |
|```--nparse <int>```                             |```-1```  |Search only N-best parse trees for each sentence (A negative value means all trees) |                      |
|```--ntypecheck <int>```                         |```-1```  |Search only N-best diagrams for each type checking of a logical form (A negative value means all diagrams) |
|```--nproof <int>```                             |```-1```  |Search only N-best diagrams for each proof search (A negative value means all diagrams) |
|```--maxdepth <int>```                           |```9```   |Set the maximum search depth in proof search (default: 9) |
|```--noTypeCheck```                              |          |If specified, show no type checking diagram for each sentence.|
|```--noInference```                              |          |If specified, execute no inference for each discourse.|
|```--time```                                     |          |Show the execution time in stderr.|
|```--verbose```                                  |          |Show type infer/check logs in stderr.|

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
