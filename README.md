# assignment-one

Haskell project for assignment-one
Tic Tac Toe game with rotation mechanic and CPU to play against.

* Doctests

* Four different flags to start with: hm (human vs machine), mm (machine vs machine), hh (human vs human) and help.


Example: -stack run hm-

Ex: -stack run help-

-stack run- inside root to run the program

-stack test- inside root to run the doctests

-${number} ${direction} to place and rotate. EX: -5 right- -2 left-

Did not get the time to implement the choosing of marks/symbols functionality, however the way I would implement it would be to create a function that returns the opposite "Symbol" (mark), and send this as the paramter to my game loop functions, as they already support this. This way you could choose the mark to play as. The way it sits now they are hard coded.

## `Main`
* Functions for command line arguments

## `Iofuncs` module
* Contains all "dirty" IO code

## `Lib` module
* Contains the pure functions

## Time logging
Thinking about various problems and designs: 7-12 hours

searching for things: 4-7 hours

Own coding/documenting: ~25 hours

## Collaboration and sources
Discussed some of the problems and shared small snippets with Sander Fuhr

Also took inspiration from

https://github.com/nt591/haskell-playground/blob/master/random/tictactoe.hs

https://gist.github.com/nick-paul/6a7df0112fe342ee828c9f6bd44a41ad