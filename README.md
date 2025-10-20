# Sudoku solver
A Sudoku solver implemented in Haskell using a backtracking algorithm.
This project was designed to support my learning for IO and monadic operations, as well as programming recursively.

## Overview
The solver is able to:
- Solve Sudoku puzzles from string input
- Read puzzles from files
- Validate solutions

## Backtracking Algorithm

The backtracking algorithm is as follows.
While the grid is empty:
1. Find an empty cell in the grid
2. Try placing numbers 1-9 in this cell
3. For each number:
   - Check if it's valid in current position
   - If so, place it and recursively try to solve the rest of the puzzle
   - If not, backtrack and try next number
4. If no number works, return to previous cell and try next number

## Setup
### Prerequisites

- GHC, version 9.6 or greater.
- Cabal, version 3.0 or greater.

### Building

```bash
# Clone the repository
git clone https://github.com/ac10022/sudoku_solver.git
cd sudoku

# Build with Cabal
cabal build

# Run tests
cabal test
```

### Core functions
The portable sudoku format concatenates the 2D 9x9 grid and replaces empty cells with 0s.
```haskell
solveGridFromString :: String -> Maybe Grid
-- Solves a Sudoku puzzle from string input (must be portable sudoku format, as above)

readGridFromFile :: FilePath -> IO (Maybe Grid)
-- Reads a Sudoku puzzle from a file

checkSudoku :: Maybe Grid -> Bool
-- Validates a solved Sudoku puzzle

processFile :: FilePath -> IO ()
-- Processes a file containing a puzzle and saves solution to a new file

showGrid :: Maybe Grid -> IO ()
-- Pretty prints a Sudoku grid
```

### Example usage

```haskell
import Sudoku

-- Solve from a string
let puzzle = "080200400570000100002300000820090005000715000700020041000006700003000018007009050"
let solution = solveGridFromString puzzle
```

### License
This project is licensed under the BSD 3-Clause License - see the LICENSE file for details.
