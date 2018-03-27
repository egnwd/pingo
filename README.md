# Pingo 🐧

This module provides functions that allows easy parsing and printing of Clingo (<https://github.com/potassco/clingo>) output.
The aim is to then sort, highlight and analyse interesting pieces of information in the Answer Sets.

Here is some example Clingo output for the first turn of a simplifed, chess-like game:

```
clingo version 5.2.1
Reading from background.lp ...
Solving...
Answer: 1
piece_at(piece(white),cell(5,1),1) piece_at(piece(white),cell(5,2),1) piece_at(piece(white),cell(5,3),1) piece_at(piece(white),cell(5,4),1) piece_at(piece(white),cell(5,5),1) piece_at(piece(black),cell(1,1),1) piece_at(piece(black),cell(1,2),1) piece_at(piece(black),cell(1,3),1) piece_at(piece(black),cell(1,4),1) piece_at(piece(black),cell(1,5),1) piece_at(piece(black),cell(1,5),2) piece_at(piece(black),cell(1,4),2) piece_at(piece(black),cell(1,3),2) piece_at(piece(black),cell(1,2),2) piece_at(piece(black),cell(1,1),2) piece_at(piece(white),cell(5,5),2) piece_at(piece(white),cell(5,4),2) piece_at(piece(white),cell(5,3),2) piece_at(piece(white),cell(5,2),2) piece_at(piece(white),cell(4,1),2)
Answer: 2
piece_at(piece(white),cell(5,1),1) piece_at(piece(white),cell(5,2),1) piece_at(piece(white),cell(5,3),1) piece_at(piece(white),cell(5,4),1) piece_at(piece(white),cell(5,5),1) piece_at(piece(black),cell(1,1),1) piece_at(piece(black),cell(1,2),1) piece_at(piece(black),cell(1,3),1) piece_at(piece(black),cell(1,4),1) piece_at(piece(black),cell(1,5),1) piece_at(piece(black),cell(1,5),2) piece_at(piece(black),cell(1,4),2) piece_at(piece(black),cell(1,3),2) piece_at(piece(black),cell(1,2),2) piece_at(piece(black),cell(1,1),2) piece_at(piece(white),cell(5,4),2) piece_at(piece(white),cell(5,3),2) piece_at(piece(white),cell(5,2),2) piece_at(piece(white),cell(5,1),2) piece_at(piece(white),cell(4,5),2)
SATISFIABLE

Models       : 2+
Calls        : 1
Time         : 0.030s (Solving: 0.00s 1st Model: 0.00s Unsat: 0.00s)
CPU Time     : 0.028s
```

The interesting information is all on one line,
Pingo extracts this information and pretty prints it.
The printing can be colored or plain.

Pingo can also parse and output clingo files,
if you wish to do any preprocessing of your logic programs.

## CLI

Usage:
```
clingo [opts] file1 ... fileN | pingo [--color=(auto|never|always)]
```

## Install

run: `cabal install`

## Documentation

run `cabal haddock`


## Library

There is also a library that parses [ASP Core 2](https://www.mat.unical.it/aspcomp2013/files/ASP-CORE-2.03b.pdf) syntax,
as well as [ILASP](https://www.doc.ic.ac.uk/~ml1909/ILASP) syntax for learning tasks.

NOTE: There are a few parts of the grammar that are not yet covered,
such as disjunctions and some aggregates.

### Disclaimer

This project is quite experimental and may change at any time.
Though I hope it should be fairly stable soon.
