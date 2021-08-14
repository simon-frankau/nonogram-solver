# Nonogram solver

This is intended to be an implementation of a
[Nonogram](https://en.wikipedia.org/wiki/Nonogram) (aka Picross)
solver. I started trying to solve them by hand recently, and decided
it would be fun to create a solver.

## How to use

The program accepts input on stdin, and returns all the possible
solutions on stdout. No possible solutions will lead to empty output.

The input should be divided into three sections, with the sections
separated by lines containing just `--`. Each line within a section is
a list of numbers separated by spaces.

The first section is a single line containing two numbers, the width
and the height. The second section is the row constraints. The number
of lines should match the height. The third section is the height
constraints. The number of lines should match the width.

In each constraint section, each line represents a constraint for the
associated row or column, from top-to-bottom and left-to-right. The
constraints are a list of numbers, each representing a block of filled
cells of the given length, each of which will be seperated by one or
more spaces.

For example `1 3 2` represents a row/column that has zero or more
blank spaces, then a single filled block, one or more spaces, three
filled blocks, one or more spaces, two filled blocks, and finally zero
or more spaces.

An empty line means there are no blocks filled in on that row. A zero
in the constraints section is not allowed.

To give a concrete example, with the input

```
3 2
--
1 1
1
--
2

1
```

produces the output:

```
X.X
X..
```

This is because the constraint `1 1` on the first line boils down to
that row being `X.X`. The second line has one block in it somewhere,
and the column constraints tell us there are two blocks in the first
column and one in the last. There is a single unique solution.

This input has two solutions:

```
2 2
--
1
1
--
1
1
```

There's a single filled entry in each row and column, giving two
possibilities that the solver finds:

```
X.
.X

.X
X.
```

## Implementation

The algorithm has two layers:

 * A constraint solver, that tries to constrain down the possible
   solutions until there's only one left (if possible).
 * A recursive wrapper, that runs the constraint solver, and if the
   constraint solver gets stuck, tries all of the remaining possibilities
   for one row, recursing in to try the solver on each.

The constraint solver starts by converting the input-style constraints
into a list of actual possible block configurations for that
row/column. This has the possibility for exponential blow-up, but
doesn't look to be a problem for real-world inputs, so I'm ignoring
that. :)

After that, we iterate two passes:

 * For each row and column, intersecting all the remaining possible
   solutions to find out which cells *must* be filled in, and which
   *must* be blank.
 * Thin out the remaining possible solutions, based on which cell are
   known filled in, and which are known blank.

If applied to just to rows or columns alone, this algorithm wouldn't
make progress, but the two-step approach propagates information
between the row and column constraints.

If you want to understand the algorithm in action, just bung a few
`println!`s inside the algorithm. :)

## Limitations and future work

 * To keep the binary operations simple, it only supports grids up to
   64x64 in size.
 * It would be nice to know for sure whether or not the constraint
   solver can solve all puzzles with a unique solution on its own,
   without relying on the recursive case split to deal with tough
   cases.
