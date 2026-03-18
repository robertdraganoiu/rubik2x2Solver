# 2x2 Rubik's Cube Solver

A Haskell solver for the 2x2 Rubik's Cube using bidirectional BFS.

## How It Works

The solver models the cube as a 3D array of cubies, each storing three face colors. It explores the state space from both the scrambled state and the solved state simultaneously using **bidirectional breadth-first search**, meeting in the middle for significantly faster solve times. Solutions are then simplified by merging consecutive moves of the same type (e.g. `R R` → `R2`, `R R'` → nothing).

### Architecture

| Module | Purpose |
|---|---|
| `ProblemState.hs` | Generic typeclass for search problems (states, successors, goal test) |
| `Cube.hs` | Cube representation, move definitions (R/U/F + inverses + doubles), solved state construction |
| `Solver.hs` | Bidirectional BFS, state space generation, path extraction |
| `Utils.hs` | Helper utilities |
| `Main.hs` | CLI interface with two input modes |

### Move Set

The solver uses the standard reduced move set for 2x2: **R, R', R2, U, U', U2, F, F', F2** (right, up, and front faces). Since one corner is always fixed on a 2x2, only three faces need to be turned.

## Usage

### Compile

```bash
ghc -O2 Main.hs -o solver
```

### Run

```bash
./solver
```

You'll be prompted to choose an input mode:

#### Option `s` — Solve from a move sequence

Enter a space-separated scramble sequence and get the solution:

```
Choose scramble option: move sequence (s) or cube colors (c):
s
Enter move sequence:
R U F R' U2 F
Solution:
F' U2 R F' U' R'
```

#### Option `c` — Solve from cube colors

Enter the 24 color values (3 per cubie, 8 cubies) describing the physical cube state. Colors are: `White`, `Yellow`, `Blue`, `Green`, `Red`, `Orange`.

Cubie order: clockwise starting from front-left, top layer first then bottom layer. For each cubie, colors are listed as: top/bottom, front/back, left/right.

```
Choose scramble option: move sequence (s) or cube colors (c):
c
Enter cubie colors clockwise, starting with front left, layers top -> bottom, colors seq top/bottom, front/back, left/right:
White Red Red White Green Orange White Orange Green White Blue Blue Yellow Green Red Yellow Blue Orange Yellow Orange Blue Yellow Red Green
Solution:
R U F2 R'
```

## Requirements

- GHC (Glasgow Haskell Compiler)
- `Data.Array` and `Data.Maybe` (included in `base`)
