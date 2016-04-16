# README

This README would normally document whatever steps are necessary to get your application up and running.

## What is this repository for?

* A Japanese CCG parser with DTS representations 
* Version 0.1.1.0
* [Learn Markdown](https://bitbucket.org/tutorials/markdowndemo)

## How do I get set up?

### Prerequisite: Haskell Platform
  1. GHC/cabal (>= version 7.0)
    * Debian: `sudo apt-get install ghc`
    * Mac: 

### Prerequisite: command-line tools
  1. juman (>= version 5.0)
    * Debian: `sudo apt-get install juman`
    * Mac:
  1. nkf (only for handling EUC-code for juman)
    * Debian: `sudo apt-get install nkf`
    * Mac:
  1. tidy (only for prettyprinting XML outputs)
    * Debian: `sudo apt-get install tidy`
    * Mac:

### Download
Do the following in the directory under which you'd like to install *lightblue*.
```
#!shell
git clone git@bitbucket.org:DaisukeBekki/lightblue.git
```
Then the directory *lightblue* (we will call this directory as <lightblue> in the following) will be created under the directory in which you did the above.

### Configuration
The following command checks dependencies.
```
#!shell
cabal configure
```
If 

### Build
```
#!shell
cabal build
```

### Generating the Document
```
#!shell
cabal haddock
```
The generated document is found at: `<lightblue>/dist/doc/html/lightblue/index.html`

### Installation

```
#!shell
cabal install
```
If succeeded, executable is found at `<home>/.cabal/bin/lightblue` and `<home>/.cabal/bin/lightbluetest`

### How to run tests
### Deployment instructions

## Contribution guidelines ###

* Writing tests
* Code review
* Other guidelines

## Who do I talk to? ###

* Repo owner or admin
* Other community or team contact