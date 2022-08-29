# FreeCell.hs
A terminal-based [FreeCell] solitaire game written in Haskell

[FreeCell]: https://en.wikipedia.org/wiki/FreeCell

### Command format

Each column is labeled by a number `1`---`8`.  The cells are labeled by `c1`---`c4` and the foundation is labeled by `f`.  To move card(s), just type

`source target`

For example, to move cards from column 2 to column 4 just type `2 4` at the prompt.  To move the bottom card of column 2 to a free cell type `2 c`.  To move the card in the third cell to column 5 type `c3 5`.  To move the bottom card of column 6 to the foundation type `6 f`, etc. Useless cards are automatically moved to the foundation, and moving to the foundation is irreversible.

When moving to an empty column, an additional number must be supplied specifying the number of cards to be moved.

Note that undo is not supported yet.

### Screenshot

![Screenshot](https://user-images.githubusercontent.com/3627229/91636380-9a86a080-ea32-11ea-9d53-3cb139cf1a03.png)
