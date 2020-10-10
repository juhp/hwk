# hwk ![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)

hwk was originally written by Lukas Martinelli in 2016-2017:
see the [original README file](README.md.orig).

<img align="right" alt="hwk" src="hwk.png" />

**hwk** tries to demonstrate how a modern Haskell based stream manipulation tool could look like.
It is similar to tools like **awk** or **sed**.
`hwk` allows compact function sequences that operate on a list of strings. Because Haskell is lazy and has a powerful arsenal of functions, there is no need to invent another DSL and hopefully it encourages more people to think functionally.

## Example

Prepend a string to each line:
```bash
$ seq 1 10 | hwk 'map ("number " ++)'
```

Sum all negative numbers
```
seq -100 100 | hwk 'sum . filter (< 0) . ints'
```
The ints function transforms a list of strings into a list of ints

Get the first two columns of a tsv file
```
cat data.tsv | hwk 'map (take 2 . splitOn "\t")'
```
(import a module defining splitOn from the extra or split library).

The argument passed to `hwk` must be a valid Haskell function: a function that takes a list of strings and returns a new list or a single value.

## Configuration
It uses a configuration module Hwk which provides the context for the hint evaluation of the supplied function.

It searches for the Hwk.hs in `~/.config/hwk`, then the package's installed datadir.
You can copy the installed Hwk.hs or source data/Hwk.hs to ~/.config/hwk to configure hwk.

The default configuration [Hwk module](data/Hwk.hs) imports
the `Prelude`, `Data.List`, and `Data.Char` modules by default to hint.

If you want to use other modules or define your own functions you can copy `data/Hwk.hs` to `~/.config/hwk/Hwk.hs`.

## Install
Either use `install.sh`, or install by cabal-install or stack
as described below.

### Install script from source tree or git
Use `stack unpack hwk` or `git clone https://github.com/juhp/hwk`.

Then go to the source directory and run the `install.sh` script, which

- first runs `stack install`
- then moves the binary installed by `stack install` to `~/.local/bin/hwk-bin`, and sets up a wrapper script `~/.local/bin/hwk` which runs it.
- and also copies the Hwk.hs configuration module to `~/.config/hwk/Hwk.hs` (backing up any existing file).

You may wish to change the resolver in stack.yaml first, which is also use to determine the resolver used by the created `hwk` wrapper script.

### cabal
If you are on a Linux distro with a system installed ghc and Haskell libaries,
you can install with `cabal install` to make use of them.

If you install with a recent cabal the Hwk.hs config module probably lives somewhere like `~/.cabal/store/ghc-*/hwk-*/share/data/Hwk.hs`, or you can copy it from the source `data/Hwk.hs`.

### stack
Installing by stack is better if you do not have a system ghc
and/or system/global Haskell libraries installed.

Alternatively to install by hand: run `stack install`,
and then run it with `stack exec hwk ...` using the same resolver,
To customize hwk after a stack install it is probably easier just to copy
the `data/Hwk.hs` source file.

## How does `hwk` work?

- `hwk` use the hint library to evaluate haskell functions on standard input.
- By default it splits the input to a list of lines: `[String] -> ToList a`
- Use `-a` or `--all` to apply a function to all the input: `String -> Tolist a`

## Supported return types

By default the following instances of the `ToList` class are defined:

- `String`
- `[String]`
- `[[String]]`
- `Int`
- `[Int]`

## Contribute

Open issue and pull requests at https://github.com/juhp/hwk
to report problems and make suggestions and contributions.

## Related/alternative projects

- https://github.com/gelisam/hawk
- https://code.google.com/p/pyp/
- https://en.wikipedia.org/wiki/AWK
- https://en.wikipedia.org/wiki/Sed
- https://github.com/bawolk/hsp
