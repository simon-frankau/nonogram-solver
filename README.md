# Nonogram solver

This is intended to be an implementation of a
[Nonogram](https://en.wikipedia.org/wiki/Nonogram) (aka Picross)
solver. I started trying to solve them by hand recently, and decided
it would be fun to create a solver.

## Limitations

 * To keep the binary operations simple, it only supports grids up to
   64x64 in size.
 * The solver does not do any "case split" logic, but simply applies
   constraints repeatedly until no further progress can be made. This
   means it assumes a unique solution, and may not solve all puzzles.

(TODO: This is based on my plans. To be implemented!)
