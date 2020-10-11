# hwk ![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)

hwk was originally written by Lukas Martinelli in 2016-2017:
see the [original README file](README.md.orig).

<img align="right" alt="hwk" src="hwk.png" />

**hwk** (pronounced "hawk") is a simple Haskell-based text stream manipulation tool, somewhat similar to tools like **awk** or **sed**.
`hwk` applies concisely composed pure functions to a list of strings from stdin. Because Haskell is lazy and has a powerful arsenal of functions, there is no need to invent another DSL. Hopefully this tool will also encourage more people to think functionally.

## Example

Prepend a string to each line:
```bash
$ seq 0 2 | hwk 'map ((++ ".txt") . show . (+100) . int)'
100.txt
101.txt
102.txt
```

Sum all negative numbers:
```bash
$ seq -100 100 | hwk 'sum . filter (< 0) . ints'
-5050
```
The ints function transforms a list of strings into a list of ints

Factorials in your shell scripts!:
```bash
seq 10 12 | hwk 'let {fact 0 = 1; fact n = n * fact (n - 1)} in map (fact . int)'
3628800
39916800
479001600
```

Extract data from a file:
```bash
$ cat /etc/passwd | hwk 'take 3 . map (filter (/= "x") . take 3 . splitOn ":")'
root	0
bin	1
daemon	2
```
(a module defining `splitOn` from the extra or split library needs to be added to the Hwk.hs config file).

The argument passed to `hwk` must be a valid Haskell function: a function that takes a list of strings and returns a new list or a single value.

## Configuration
It uses a configuration module `Hwk` which provides the context for the hint evaluation of the supplied function.

It searches for `Hwk.hs` in `~/.config/hwk`, then the package's installed data directory.

The default configuration [Hwk module](data/Hwk.hs) just sets
the `Prelude`, `Data.List`, and `Data.Char` modules to be imported by default into the hint interpreter.

If you want to use other modules or define your own functions, you can copy the installed `Hwk.hs` or source `data/Hwk.hs` file to `~/.config/hwk/` to configure hwk.

## Install
Either use the `install.sh` script, or install by cabal-install or stack
as described below:

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
and/or global system Haskell libraries installed.

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

Open an issue or pull request at https://github.com/juhp/hwk
to report problems or make suggestions and contributions.

## Related/alternative projects

- https://github.com/gelisam/hawk
- https://github.com/bawolk/hsp
- https://code.google.com/p/pyp/
- https://en.wikipedia.org/wiki/AWK
- https://en.wikipedia.org/wiki/Sed
