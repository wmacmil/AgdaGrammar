# AgdaGrammar

## Overview

This repository is the source for my Master's Thesis, prematurely titled "A Grammar of Proof".  It is a project concerned with the syntax of mathematical propositions and proofs, and other ingriedients which mathematicians take for granted : definitions, lemma, corollary, example, and auxilliary comments.  The goals are manifold: 

- Capture essential mathematical concepts that are expressible via both machines and natural language
- Provide a bidirection means of translation, via Grammatical Framework (GF) between a given machine syntax and a controlled natural langauge syntax approximating a mathematician's daily vocabulary
- Provide evidence that GF is a promising and possibly adequate approach to this task, via some case studies from concepts in type theory : Booleans, Natural Numbers, Propositional Logic, and Homotopy Type Theory.

The machine language of choice is Agda, of which we'll only be able to capture an approximation, but it will ideally be extended to Coq and Lean.  

Previous work in this direction is attributed to my advisor, [Aarne Ranta](http://www.cse.chalmers.se/~aarne/)
He made a preliminary GF grammar for parsing [homotopy type theory](https://github.com/GrammaticalFramework/gf-contrib/tree/master/homotopy-typetheory), as well as a paper [Translating between language and logic: what is easy and what is difficult](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.230.9739), whose code is available [here](https://github.com/GrammaticalFramework/gf-contrib/tree/master/cade-2011).

## Contents

- Exp.gf 
  + Abstract syntax specification which essentially encodes Agda's LF + Identity Types.  This is largely a GFification of the non-cubical abstract snytax of the bnfc file for [CubicalTT](https://github.com/mortberg/cubicaltt).
- ExpCubicalTT.gf
  + Concrete syntax representation with syntax similair to CubicalTT.
- ExpAgda.gf
  + Concrete syntax for Agda.
- test.sh
  + test script. currently parses various CubicalTT sentences.
- /haskell
  + generating variable list.
- /agda
  + Id.agda
    * code which syntactically corresponds to much of chapter 2 of the HoTT book
  + Bool.agda
    * agda code for booleans, to be used for testing
- /textfiles
  + various manipulations of ASTs as primitive testing mechanism (should eventually be deleted)

## TODO

- ExpPigdinEng.gf 
  + Concrete syntax for English, but without RGL and any linguistic nuance.
- ExpEng.gf 
  + Concrete English syntax with RGL.
  + \*.latex 
- More robust testing suite
- Interface with PGF to manipulate ASTs
- Pre/Post procesing to deal with whitespace
- Test cases. Most if not all of these should soon support both Agda and NL implementations
  + Booleans
  + Natural Numbers
  + Lists
  + Propositional & FOL
  + HoTT (as corresponds to the Id.agda file)
- Thesis writing itself.
