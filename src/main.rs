// Nonogram solver
//
// Copyright 2021 Simon Frankau
//

use std::fmt;
use std::fmt::Write;
use std::io;
use std::io::Read;
use std::mem;
use std::str::FromStr;

#[cfg(test)]
use std::collections::HashSet;

////////////////////////////////////////////////////////////////////////
// Input parsing
//

const PARSE_ERR_SECTIONS: &str = "Expected three sections separate by lines containing '--'.";

const PARSE_ERR_SEC1: &str = "Section 1 should be a single line containing 2 numbers.";

#[derive(Debug, Eq, PartialEq)]
struct Input {
    rows: Vec<Vec<usize>>,
    cols: Vec<Vec<usize>>,
}

// Input is divided into three sections with lines containing just '--'
// (why not a blank line? As that's a valid line in sections 2 and 3).
//
// Each section is a number of lines. Each line contains 0 or more
// integers, separated by spaces.
//
// The sections are as follows:
//
// 1. A single line, containing 2 numbers. These are the width and
//    height of the grid respectively, and thus the number of rows
//    in sections 2 and 3.
//
// 2. The horizontal constraints, as written in the puzzles. There is
//    one line per row, and an empty row represents no filled-in blocks
//    on that row. Each number represents a number of filled in squares
//    in that block (left-to-right), each block is separated by at least
//    one blank square.
//
// 3. Vertical constraints, in the same style as #2, running
//    top-to-bottom. A final '\n' is allowed - newlines may be
//    line terminators, rather than separators.
impl FromStr for Input {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Add line numbers to make errors more understandable.
        let lines: Vec<(&str, usize)> = s.split('\n').zip(1..).collect();

        let section_markers = lines
            .iter()
            .filter_map(|&(s, line_num)| {
                if s.trim() == "--" {
                    Some(line_num)
                } else {
                    None
                }
            })
            .collect::<Vec<usize>>();

        if section_markers.len() != 2 {
            return Err(PARSE_ERR_SECTIONS.to_string());
        }

        // We have our sections, let's start parsing!

        fn parse_line(line: (&str, usize)) -> Result<Vec<usize>, String> {
            let (line, line_num) = line;
            line.trim()
                .split_whitespace()
                .map(|xs| {
                    xs.parse::<usize>()
                        .map_err(|e| format!("Line {}: {}", line_num, e))
                })
                .collect::<Result<Vec<_>, String>>()
        }

        // First section should be single line...
        if section_markers[0] != 2 {
            return Err(format!("Line 2: {}", PARSE_ERR_SEC1));
        }

        let (width, height) = {
            let xs = parse_line(lines[0])?;
            if xs.len() != 2 {
                return Err(format!("Line 1: {}", PARSE_ERR_SEC1));
            }
            (xs[0], xs[1])
        };

        // Parse section 2:
        let section2 = &lines[section_markers[0]..section_markers[1] - 1];
        if section2.len() != height {
            return Err(format!(
                "Line {}: Expected {} rows, found {} in section 2",
                section_markers[0] + 1,
                height,
                section2.len()
            ));
        }
        let rows = section2
            .iter()
            .map(|&line| parse_line(line))
            .collect::<Result<Vec<Vec<usize>>, String>>()?;

        // Parse section 3, which is like section 2, with an optional
        // terminating newline.
        let mut section3 = &lines[section_markers[1]..];
        let s3_len = section3.len();
        if s3_len == width + 1 && section3[s3_len - 1].0.trim() == "" {
            // Remove optional trailing newline.
            section3 = &section3[..s3_len - 1];
        }

        if section3.len() != width {
            return Err(format!(
                "Line {}: Expected {} columns, found {} in section 3",
                section_markers[1] + 1,
                width,
                section3.len()
            ));
        }
        let cols = section3
            .iter()
            .map(|&line| parse_line(line))
            .collect::<Result<Vec<Vec<usize>>, String>>()?;

        Ok(Input {
            rows: rows,
            cols: cols,
        })
    }
}

////////////////////////////////////////////////////////////////////////
// Bitmap generation and printing
//

// Generate a text representation of a bitmap.
//
// n is the number of bits actually being used.
#[cfg(test)]
fn bitmap_to_string(bitmap: u64, n: usize) -> String {
    let mut acc = String::new();
    for i in (0..n).rev() {
        acc.push(if (bitmap >> i) & 1 != 0 { 'X' } else { '.' });
    }
    acc
}

// Given a list of numbers representing the size of fille-in blocks
// (as written on the puzzle), produce a list of bitmaps representing
// the possible ways of filling the blocks to meet the constraint.
//
// The returned bitmap has the MSB end representing left/top, and is
// LSB-aligned (i.e. the LSB represents right/bottom, the MSB will
// only be used for 64 cell puzzles).
//
// I believe this algorithm may produce output size exponential in n
// and counts.len(), but... meh. In practice counts.len() doesn't tend
// to be huge, and modern computers are fast with lots of RAM.
fn expand(counts: &[usize], n: usize) -> Vec<u64> {
    assert!(n <= 64, "Width and height must be less than or equal to 64");

    // Annoying special case - only case where the minimum number of
    // blank blocks needed is not counts.len() - 1. Deal with it
    // early.
    if counts.is_empty() {
        return vec![0]; // One possibility, all empty.
    }

    let filled_blocks: usize = counts.iter().sum();
    let min_blanks = counts.len() - 1;

    assert!(filled_blocks + min_blanks <= n);
    assert!(!counts.iter().any(|&x| x == 0));

    // Auxiliary function used for recursion - takes expansion so far,
    // extends it with remaining counts/n, and pushes results into the
    // accumulator.
    fn expand_aux(counts: &[usize], num_blanks: usize, so_far: u64, accumulator: &mut Vec<u64>) {
        // Base case.
        if counts.is_empty() {
            let curr = so_far << num_blanks;
            accumulator.push(curr);
            return;
        }

        // 1. Add optional blank space.
        for i in 0..=num_blanks {
            let mut curr = so_far << i;

            // 2. Add required block.
            curr = !curr;
            curr <<= counts[0];
            curr = !curr;
            let new_counts = &counts[1..];

            // 3. Add required blank space if there's another block.
            if !new_counts.is_empty() {
                curr <<= 1;
            }

            // 4. And repeat
            expand_aux(new_counts, num_blanks - i, curr, accumulator);
        }
    }

    let mut accumulator = Vec::new();
    // spare_spaces are the number of spaces excluding those *required*
    // between blocks.
    let spare_spaces = n - filled_blocks - min_blanks;
    expand_aux(counts, spare_spaces, 0, &mut accumulator);
    accumulator
}

////////////////////////////////////////////////////////////////////////
// Solving logic
//

#[derive(Debug, Eq, PartialEq)]
enum ConstraintSolverFailure {
    // There's some kind of contradiction, meaning no solution is possible.
    NoSolutions,
    // We can't make further progress. Either this is a tricky case the
    // constraint solved can't handle, or there are multiple solutions,
    // meaning we can't find a unique path forwards.
    Stuck,
}

// Constrain the possibilities list, based on the known filled and blank
// cells. Any solution that clashes with known cells is discarded.
fn constrain_poss(
    poss: &[u64],
    known_filled: u64,
    known_blank: u64,
) -> Result<Vec<u64>, ConstraintSolverFailure> {
    let res: Vec<u64> = poss
        .iter()
        .filter(|&x| x & known_blank == 0 && !x & known_filled == 0)
        .cloned()
        .collect();

    if res.is_empty() {
        Err(ConstraintSolverFailure::NoSolutions)
    } else {
        Ok(res)
    }
}

// Constrain the known filled/blank list. If all possible solutions
// agree on a cell, it becomes known filled/blank. This should be
// a monotonic increase in the known values, with no clashes.
fn constrain_known(poss: &[u64], n: usize, known_filled: u64, known_blank: u64) -> (u64, u64) {
    // Build a mask containing only the bits we care about.
    //
    // Made complex by handling n == 64.
    let initial_mask = 1u64.checked_shl(n as u32).map(|x| x - 1).unwrap_or(!0);

    let new_known_filled: u64 = poss.iter().fold(initial_mask, |acc, x| acc & x);

    let new_known_blank: u64 = poss.iter().fold(initial_mask, |acc, x| acc & !x);

    // We only shrink the set of possibilities for a row/col, so we
    // should only ever grow the set of known values. Furthermore,
    // it should be impossible for all the possibilities to have
    // the same bit set and unset (unless the possibility set is
    // empty, which should alredy be caught in constrain_poss).
    assert!(
        known_filled & new_known_filled == known_filled
            && known_blank & new_known_blank == known_blank
            && new_known_filled & new_known_blank == 0,
        "Internal error"
    );

    (new_known_filled, new_known_blank)
}

#[derive(Debug, Clone)]
struct Solver {
    width: usize,
    height: usize,
    // For each row and column, the solutions that remain possible.
    poss_rows: Vec<Vec<u64>>,
    poss_cols: Vec<Vec<u64>>,
    // The solution so far. A bitmap per row, with bit set if that
    // cell is known filled or blank.
    known_filled: Vec<u64>,
    known_blanks: Vec<u64>,
}

impl Solver {
    fn from_input(input: &Input) -> Solver {
        let w = input.cols.len();
        let h = input.rows.len();

        Solver {
            width: w,
            height: h,
            poss_rows: input.rows.iter().map(|v| expand(v, w)).collect(),
            poss_cols: input.cols.iter().map(|v| expand(v, h)).collect(),
            known_filled: vec![0; h],
            known_blanks: vec![0; h],
        }
    }

    // Return number of alternatives in the solution. Used to check that
    // this is reducing every iteration, so that we're converging. Zero
    // means we have a unique solution
    fn size(&self) -> usize {
        let row_alts: usize = self.poss_rows.iter().map(|x| x.len()).sum();
        let col_alts: usize = self.poss_cols.iter().map(|x| x.len()).sum();

        row_alts + col_alts - self.width - self.height
    }

    // Apply constraints in the horizontal direction
    fn constrain_h(&mut self) -> Result<(), ConstraintSolverFailure> {
        self.poss_rows = self
            .poss_rows
            .iter()
            .zip(self.known_filled.iter())
            .zip(self.known_blanks.iter())
            .map(|((row, &filled), &blank)| constrain_poss(&row, filled, blank))
            .collect::<Result<Vec<_>, ConstraintSolverFailure>>()?;

        let new_filled_and_blanks = self
            .poss_rows
            .iter()
            .zip(self.known_filled.iter())
            .zip(self.known_blanks.iter())
            .map(|((row, &filled), &blank)| constrain_known(&row, self.width, filled, blank))
            .collect::<Vec<_>>();

        self.known_filled = new_filled_and_blanks
            .iter()
            .map(|(filled, _)| *filled)
            .collect::<Vec<_>>();
        self.known_blanks = new_filled_and_blanks
            .iter()
            .map(|(_, blank)| *blank)
            .collect::<Vec<_>>();

        Ok(())
    }

    fn transpose(&mut self) {
        fn transpose_bitvec(width: usize, rows: &[u64]) -> Vec<u64> {
            let mut v = Vec::new();
            for col_num in (0..width).rev() {
                let mut col: u64 = 0;
                for row in rows.iter() {
                    col <<= 1;
                    col |= (row >> col_num) & 1;
                }
                v.push(col);
            }
            v
        }

        self.known_filled = transpose_bitvec(self.width, &self.known_filled);
        self.known_blanks = transpose_bitvec(self.width, &self.known_blanks);

        mem::swap(&mut self.width, &mut self.height);
        mem::swap(&mut self.poss_rows, &mut self.poss_cols);
    }

    // Apply constraints vertically
    fn constrain_v(&mut self) -> Result<(), ConstraintSolverFailure> {
        self.transpose();
        let res = self.constrain_h();
        self.transpose();
        res
    }

    // Perform a constraining step
    fn constrain(&mut self) -> Result<(), ConstraintSolverFailure> {
        self.constrain_h()?;
        self.constrain_v()?;
        Ok(())
    }

    // And find a unique solution by iteratively constraining until
    // a solution is found. I don't thinks we're guaranteed to find a
    // solution if it exists, but I have yet to find a counterexample.
    fn solve_constraints(&mut self) -> Result<(), ConstraintSolverFailure> {
        // Do an initial pass, since it might not reduce the possibility
        // sets (which we check for progress), but might fill in a few
        // initial known entries.
        self.constrain()?;
        let mut last_size = self.size();

        while last_size != 0 {
            self.constrain()?;
            let size = self.size();
            if last_size == size && size != 0 {
                return Err(ConstraintSolverFailure::Stuck);
            }
            last_size = size;
        }

        Ok(())
    }
}

impl fmt::Display for Solver {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (filled, blank) in self.known_filled.iter().zip(self.known_blanks.iter()) {
            for i in (0..self.width).rev() {
                let c = match ((filled >> i) & 1, (blank >> i) & 1) {
                    (0, 0) => '?',
                    (1, 0) => 'X',
                    (0, 1) => '.',
                    (_, _) => panic!("Internal error"),
                };
                f.write_char(c)?;
            }
            f.write_char('\n')?;
        }
        Ok(())
    }
}

// Attempt to find all solutions, by performing a constraint-solving pass
// and any time we get stuck, we perform a case split and recurse.
fn solve_recursive(solver: Solver) -> Vec<Solver> {
    fn choose_split_line(solver: &Solver) -> usize {
        // We choose the row (ignoring columns for simplicity) with the
        // fewest alternatives, assuming (unfoundedly) that leads to
        // less searching.
        solver
            .poss_rows
            .iter()
            .map(|r| r.len())
            .zip(0..)
            .filter(|(l, _)| *l != 1)
            .min()
            .unwrap()
            .1
    }

    fn solve_recursive_aux(mut solver: Solver, acc: &mut Vec<Solver>) {
        match solver.solve_constraints() {
            Ok(()) => acc.push(solver),
            Err(ConstraintSolverFailure::NoSolutions) => return,
            Err(ConstraintSolverFailure::Stuck) => {
                let i = choose_split_line(&solver);
                for poss in solver.poss_rows[i].iter() {
                    let mut rec_solver = solver.clone();
                    rec_solver.poss_rows[i] = vec![*poss];
                    solve_recursive_aux(rec_solver, acc)
                }
            }
        }
    }

    let mut v = Vec::new();
    solve_recursive_aux(solver.clone(), &mut v);
    v
}

////////////////////////////////////////////////////////////////////////
// Main entry point
//

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    let input = buffer.parse::<Input>()?;
    let solver = Solver::from_input(&input);
    let solutions: String = solve_recursive(solver)
        .iter()
        .map(|s| s.to_string())
        .collect::<Vec<_>>()
        .concat();

    println!("{}", solutions);

    Ok(())
}

////////////////////////////////////////////////////////////////////////
// Tests
//

#[cfg(test)]
mod tests {
    use super::*;

    // Parsing tests

    #[test]
    fn test_parse_sections_incorrect() {
        assert_eq!("OOPS".parse::<Input>(), Err(PARSE_ERR_SECTIONS.to_string()));
        assert_eq!(
            "OOPS\n--\nSAD\n--\nToo many\n--\n???".parse::<Input>(),
            Err(PARSE_ERR_SECTIONS.to_string())
        );
    }

    #[test]
    fn test_parse_section1_bad_line_count() {
        assert_eq!(
            "1 2\n3 4\n--\n--\n".parse::<Input>(),
            Err(format!("Line 2: {}", PARSE_ERR_SEC1))
        );
    }

    #[test]
    fn test_parse_section1_bad_element_count() {
        assert_eq!(
            "1 2 3\n--\n--\n".parse::<Input>(),
            Err(format!("Line 1: {}", PARSE_ERR_SEC1))
        );
    }

    #[test]
    fn test_parse_section1_bad_element() {
        assert_eq!(
            "1 cheese 3\n--\n--\n".parse::<Input>(),
            Err("Line 1: invalid digit found in string".to_string())
        );
    }

    #[test]
    fn test_parse_section2_bad_line_count() {
        assert_eq!(
            "1 3\n--\n7\n42 13\n--\n".parse::<Input>(),
            Err("Line 3: Expected 3 rows, found 2 in section 2".to_string())
        );
    }

    #[test]
    fn test_parse_section2_bad_element() {
        assert_eq!(
            "1 2\n--\n7\n42 what? 13\n--\n".parse::<Input>(),
            Err("Line 4: invalid digit found in string".to_string())
        );
    }

    #[test]
    fn test_parse_section3_bad_line_count() {
        assert_eq!(
            "4 2\n--\n7\n42 13\n--\n17".parse::<Input>(),
            Err("Line 6: Expected 4 columns, found 1 in section 3".to_string())
        );
    }

    #[test]
    fn test_parse_section3_bad_element() {
        assert_eq!(
            "1 2\n--\n7\n42 13\n--\n0x17".parse::<Input>(),
            Err("Line 6: invalid digit found in string".to_string())
        );
    }

    #[test]
    fn test_parse_success() {
        assert_eq!(
            "1 2\n--\n7\n42 13\n--\n17".parse::<Input>(),
            Ok(Input {
                rows: vec![vec![7], vec![42, 13]],
                cols: vec![vec![17]],
            })
        );
    }

    #[test]
    fn test_parse_trailing_newline() {
        assert_eq!(
            "1 2\n--\n7\n42 13\n--\n17".parse::<Input>().unwrap(),
            "1 2\n--\n7\n42 13\n--\n17\n".parse::<Input>().unwrap()
        )
    }

    #[test]
    fn test_parse_extra_whitespace() {
        assert_eq!(
            "1 2\n--\n7\n42 13\n--\n17".parse::<Input>().unwrap(),
            " 1  2\n -- \n7\n42\t13\n--\n17 \n"
                .parse::<Input>()
                .unwrap()
        )
    }

    // Bitmap tests

    #[test]
    fn test_bitmap_to_string_empty() {
        assert_eq!(bitmap_to_string(0, 0), "")
    }

    #[test]
    fn test_bitmap_to_string_happy_path() {
        assert_eq!(bitmap_to_string(13, 7), "...XX.X")
    }

    #[test]
    #[should_panic]
    fn test_expand_max_size() {
        expand(&vec![2], 65);
    }

    // Check a set of results matches the bitmaps expected,
    // independent of order
    fn is_equiv(bitmaps: &[u64], n: usize, expected: &[&str]) -> bool {
        let bitmap_strings = bitmaps
            .iter()
            .map(|&x| bitmap_to_string(x, n))
            .collect::<Vec<String>>();

        let bitmap_set = bitmap_strings
            .iter()
            .map(|x| x.as_str())
            .collect::<HashSet<&str>>();

        let expected_set = expected.iter().cloned().collect::<HashSet<&str>>();

        bitmap_set == expected_set
    }

    #[test]
    fn test_expand_trivial() {
        let n = 0;
        let bitmaps = expand(&Vec::new(), n);
        assert!(is_equiv(&bitmaps, n, &vec![""]));
    }

    #[test]
    fn test_expand_blank() {
        let n = 5;
        let bitmaps = expand(&Vec::new(), n);
        assert!(is_equiv(&bitmaps, n, &vec!["....."]));
    }

    #[test]
    fn test_expand_small() {
        let n = 5;
        let bitmaps = expand(&vec![1, 2], n);
        assert!(is_equiv(&bitmaps, n, &vec!["X.XX.", "X..XX", ".X.XX"]));
    }

    #[test]
    fn test_expand_medium() {
        let n = 15;
        let bitmaps = expand(&vec![1, 2, 3], n);
        let expected = vec![
            "X.XX.XXX.......",
            "X.XX..XXX......",
            "X.XX...XXX.....",
            "X.XX....XXX....",
            "X.XX.....XXX...",
            "X.XX......XXX..",
            "X.XX.......XXX.",
            "X.XX........XXX",
            "X..XX.XXX......",
            "X..XX..XXX.....",
            "X..XX...XXX....",
            "X..XX....XXX...",
            "X..XX.....XXX..",
            "X..XX......XXX.",
            "X..XX.......XXX",
            "X...XX.XXX.....",
            "X...XX..XXX....",
            "X...XX...XXX...",
            "X...XX....XXX..",
            "X...XX.....XXX.",
            "X...XX......XXX",
            "X....XX.XXX....",
            "X....XX..XXX...",
            "X....XX...XXX..",
            "X....XX....XXX.",
            "X....XX.....XXX",
            "X.....XX.XXX...",
            "X.....XX..XXX..",
            "X.....XX...XXX.",
            "X.....XX....XXX",
            "X......XX.XXX..",
            "X......XX..XXX.",
            "X......XX...XXX",
            "X.......XX.XXX.",
            "X.......XX..XXX",
            "X........XX.XXX",
            ".X.XX.XXX......",
            ".X.XX..XXX.....",
            ".X.XX...XXX....",
            ".X.XX....XXX...",
            ".X.XX.....XXX..",
            ".X.XX......XXX.",
            ".X.XX.......XXX",
            ".X..XX.XXX.....",
            ".X..XX..XXX....",
            ".X..XX...XXX...",
            ".X..XX....XXX..",
            ".X..XX.....XXX.",
            ".X..XX......XXX",
            ".X...XX.XXX....",
            ".X...XX..XXX...",
            ".X...XX...XXX..",
            ".X...XX....XXX.",
            ".X...XX.....XXX",
            ".X....XX.XXX...",
            ".X....XX..XXX..",
            ".X....XX...XXX.",
            ".X....XX....XXX",
            ".X.....XX.XXX..",
            ".X.....XX..XXX.",
            ".X.....XX...XXX",
            ".X......XX.XXX.",
            ".X......XX..XXX",
            ".X.......XX.XXX",
            "..X.XX.XXX.....",
            "..X.XX..XXX....",
            "..X.XX...XXX...",
            "..X.XX....XXX..",
            "..X.XX.....XXX.",
            "..X.XX......XXX",
            "..X..XX.XXX....",
            "..X..XX..XXX...",
            "..X..XX...XXX..",
            "..X..XX....XXX.",
            "..X..XX.....XXX",
            "..X...XX.XXX...",
            "..X...XX..XXX..",
            "..X...XX...XXX.",
            "..X...XX....XXX",
            "..X....XX.XXX..",
            "..X....XX..XXX.",
            "..X....XX...XXX",
            "..X.....XX.XXX.",
            "..X.....XX..XXX",
            "..X......XX.XXX",
            "...X.XX.XXX....",
            "...X.XX..XXX...",
            "...X.XX...XXX..",
            "...X.XX....XXX.",
            "...X.XX.....XXX",
            "...X..XX.XXX...",
            "...X..XX..XXX..",
            "...X..XX...XXX.",
            "...X..XX....XXX",
            "...X...XX.XXX..",
            "...X...XX..XXX.",
            "...X...XX...XXX",
            "...X....XX.XXX.",
            "...X....XX..XXX",
            "...X.....XX.XXX",
            "....X.XX.XXX...",
            "....X.XX..XXX..",
            "....X.XX...XXX.",
            "....X.XX....XXX",
            "....X..XX.XXX..",
            "....X..XX..XXX.",
            "....X..XX...XXX",
            "....X...XX.XXX.",
            "....X...XX..XXX",
            "....X....XX.XXX",
            ".....X.XX.XXX..",
            ".....X.XX..XXX.",
            ".....X.XX...XXX",
            ".....X..XX.XXX.",
            ".....X..XX..XXX",
            ".....X...XX.XXX",
            "......X.XX.XXX.",
            "......X.XX..XXX",
            "......X..XX.XXX",
            ".......X.XX.XXX",
        ];

        assert!(is_equiv(&bitmaps, n, &expected));
    }

    #[test]
    fn test_expand_large() {
        let n = 40;
        let bitmaps = expand(&vec![1, 2, 3, 4, 5], n);
        assert_eq!(bitmaps.len(), 65780);
    }

    // Solver tests

    #[test]
    fn test_constrain_poss_filled() {
        // Check constraining a block of 2 in 5, where the middle bit
        // must be set.
        let poss = constrain_poss(&expand(&vec![2], 5), 0b00100, 0).unwrap();
        assert!(is_equiv(&poss, 5, &vec![".XX..", "..XX."]));
    }

    #[test]
    fn test_constrain_poss_blank() {
        // Check constraining a block of 2 in 5, where the middle bit
        // must *not* be set.
        let poss = constrain_poss(&expand(&vec![2], 5), 0, 0b00100).unwrap();
        assert!(is_equiv(&poss, 5, &vec!["XX...", "...XX"]));
    }

    #[test]
    fn test_constrain_poss_filled_and_blank() {
        // Check constraining a block of 2 in 5, where the middle bit
        // must be set, and the next must not.
        let poss = constrain_poss(&expand(&vec![2], 5), 0b00100, 0b00010).unwrap();
        assert!(is_equiv(&poss, 5, &vec![".XX.."]));
    }

    #[test]
    fn test_constrain_poss_impossible() {
        // Check constraining a block of 2 in 5, where 4 bits must not
        // be set - impossible.
        assert_eq!(
            constrain_poss(&expand(&vec![2], 5), 0, 0b01111),
            Err(ConstraintSolverFailure::NoSolutions)
        );
    }

    #[test]
    fn test_constrain_known_success1() {
        // 3 in a row must involve the middle bit.
        let poss = expand(&vec![3], 5);
        assert_eq!(constrain_known(&poss, 5, 0b00100, 0), (0b00100, 0));
    }

    #[test]
    fn test_constrain_known_success2() {
        // Two blocks of 2 in 5 cells has a unique solution.
        let poss = expand(&vec![2, 2], 5);
        assert_eq!(constrain_known(&poss, 5, 0, 0), (0b11011, 0b00100));
    }

    #[test]
    #[should_panic]
    fn test_constrain_known_failure() {
        // Not monotonic - left bit isn't known filled.
        let poss = expand(&vec![3], 5);
        let _known = constrain_known(&poss, 5, 0b10000, 0);
    }

    #[test]
    fn test_solver_from_input() {
        let input = "5 3
                     --
                     1 2
                     1
                     2
                     --

                     1
                     2
                     1

                     "
        .parse::<Input>()
        .unwrap();

        let solver = Solver::from_input(&input);
        let w = 5;
        let h = 3;

        assert_eq!(solver.width, w);
        assert_eq!(solver.height, h);

        assert_eq!(solver.poss_rows.len(), h);
        assert!(is_equiv(
            &solver.poss_rows[0],
            w,
            &vec!["X.XX.", "X..XX", ".X.XX"]
        ));
        assert!(is_equiv(
            &solver.poss_rows[1],
            w,
            &vec!["X....", ".X...", "..X..", "...X.", "....X"]
        ));
        assert!(is_equiv(
            &solver.poss_rows[2],
            w,
            &vec!["XX...", ".XX..", "..XX.", "...XX"]
        ));

        assert_eq!(solver.poss_cols.len(), w);
        assert!(is_equiv(&solver.poss_cols[0], h, &vec!["..."]));
        assert!(is_equiv(
            &solver.poss_cols[1],
            h,
            &vec!["X..", ".X.", "..X"]
        ));
        assert!(is_equiv(&solver.poss_cols[2], h, &vec!["XX.", ".XX"]));
        assert!(is_equiv(
            &solver.poss_cols[3],
            h,
            &vec!["X..", ".X.", "..X"]
        ));
        assert!(is_equiv(&solver.poss_cols[4], h, &vec!["..."]));

        assert_eq!(solver.known_filled.len(), h);
        assert!(solver.known_filled.iter().all(|&x| x == 0));

        assert_eq!(solver.known_blanks.len(), h);
        assert!(solver.known_blanks.iter().all(|&x| x == 0));
    }

    #[test]
    fn test_solver_size() {
        let input = "5 3
                     --
                     1 2
                     1
                     2
                     --

                     1
                     2
                     1

                     "
        .parse::<Input>()
        .unwrap();

        let solver = Solver::from_input(&input);

        // Count row solutions, and column solutions.
        assert_eq!(solver.size(), (3 + 5 + 4) - 3 + (1 + 3 + 2 + 3 + 1) - 5);
    }

    #[test]
    fn test_solver_size_on_completion() {
        let input = "2 1
                     --
                     2
                     --
                     1
                     1
                     "
        .parse::<Input>()
        .unwrap();

        let solver = Solver::from_input(&input);

        // Already a unique solution.
        assert_eq!(solver.size(), 0);
    }

    #[test]
    fn test_transpose() -> Result<(), ConstraintSolverFailure> {
        let input = "5 3 \n --
                     1 \n \n 5 \n --
                     1 1 \n 1 \n 1 \n 1 \n 1"
            .parse::<Input>()
            .unwrap();
        // Build a solver and fill in some known_XXX constraints.
        let mut solver_before = Solver::from_input(&input);
        solver_before.constrain_h()?;

        let mut solver_after = solver_before.clone();
        solver_after.transpose();

        assert_eq!(solver_after.width, solver_before.height);
        assert_eq!(solver_after.height, solver_before.width);
        assert_eq!(solver_after.poss_rows, solver_before.poss_cols);
        assert_eq!(solver_after.poss_cols, solver_before.poss_rows);

        // Check pre-transpose for paranoia.
        assert_eq!(solver_before.known_filled, vec![0b00000, 0b00000, 0b11111]);
        assert_eq!(solver_before.known_blanks, vec![0b00000, 0b11111, 0b00000]);

        assert_eq!(
            solver_after.known_filled,
            vec![0b001, 0b001, 0b001, 0b001, 0b001]
        );
        assert_eq!(
            solver_after.known_blanks,
            vec![0b010, 0b010, 0b010, 0b010, 0b010]
        );

        Ok(())
    }

    #[test]
    fn test_solve_tiny() -> Result<(), ConstraintSolverFailure> {
        let input = "2 2
                     --
                     2

                     --
                     1
                     1
                     "
        .parse::<Input>()
        .unwrap();
        let mut solver = Solver::from_input(&input);
        solver.solve_constraints()?;
        assert_eq!(
            solver.to_string(),
            "\
XX
..
"
        );

        Ok(())
    }

    #[test]
    fn test_solve_small() -> Result<(), ConstraintSolverFailure> {
        let input = "6 6
                     --

                     1

                     2
                     2

                     --
                     1

                     2
                     2


                     "
        .parse::<Input>()
        .unwrap();
        let mut solver = Solver::from_input(&input);
        solver.solve_constraints()?;
        assert_eq!(
            solver.to_string(),
            "\
......
X.....
......
..XX..
..XX..
......
"
        );

        Ok(())
    }

    #[test]
    fn test_solve_large() -> Result<(), ConstraintSolverFailure> {
        // Full 64x64 might hit some corner cases.
        let input = "64 64
                     --
                     1
                     \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
                     \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
                     \n\n\n\n\n\n\n\n
                     2
                     2
                     --
                     1
                     \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
                     \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
                     \n\n\n\n\n\n\n\n
                     2
                     2
                     "
        .parse::<Input>()
        .unwrap();
        let mut solver = Solver::from_input(&input);
        solver.solve_constraints()?;
        assert_eq!(
            solver.to_string(),
            "\
X...............................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
..............................................................XX
..............................................................XX
"
        );

        Ok(())
    }

    #[test]
    fn test_solve_ambiguity_fail() {
        let input = "3 3
                     --
                     1

                     1
                     --
                     1

                     1
                     "
        .parse::<Input>()
        .unwrap();
        let mut solver = Solver::from_input(&input);
        assert_eq!(
            solver.solve_constraints(),
            Err(ConstraintSolverFailure::Stuck)
        );
    }

    #[test]
    fn test_solve_recursive() {
        // Check the recursive solver finds multiple solutions.
        let input = "3 3
                     --
                     1

                     1
                     --
                     1

                     1
                     "
        .parse::<Input>()
        .unwrap();
        let solver = Solver::from_input(&input);
        let found_solutions = solve_recursive(solver)
            .iter()
            .map(|s| s.to_string())
            .collect::<HashSet<String>>();

        let known_solutions: HashSet<String> = [
            "\
X..
...
..X
",
            "\
..X
...
X..
",
        ]
        .iter()
        .map(|s| s.to_string())
        .collect();

        assert_eq!(found_solutions, known_solutions);
    }

    #[test]
    fn test_solve_recursive_fail() {
        // Check the recursive solver copes with no solutions.
        let input = "3 3
                     --
                     1
                     1
                     1
                     --
                     1

                     1
                     "
        .parse::<Input>()
        .unwrap();
        let solver = Solver::from_input(&input);
        let found_solutions = solve_recursive(solver)
            .iter()
            .map(|s| s.to_string())
            .collect::<HashSet<String>>();

        assert_eq!(found_solutions.len(), 0);
    }

    #[test]
    fn test_solve_stupid_corner_case() {
        // Silly corner case: Size zero.
        let input = "0 0
                     --
                     --
                     "
        .parse::<Input>()
        .unwrap();
        let solver = Solver::from_input(&input);
        let found_solutions = solve_recursive(solver)
            .iter()
            .map(|s| s.to_string())
            .collect::<HashSet<String>>();

        let known_solutions = [""]
            .iter()
            .map(|s| s.to_string())
            .collect::<HashSet<String>>();

        assert_eq!(found_solutions, known_solutions);
    }

    #[test]
    fn test_solve_stupid_corner_case_vertical() {
        // And while I'm at it, zero size in one dimension...
        let input = "0 5
                     --





                     --
                     "
        .parse::<Input>()
        .unwrap();
        let solver = Solver::from_input(&input);
        let found_solutions = solve_recursive(solver)
            .iter()
            .map(|s| s.to_string())
            .collect::<HashSet<String>>();

        let known_solutions = ["\n\n\n\n\n"]
            .iter()
            .map(|s| s.to_string())
            .collect::<HashSet<String>>();

        assert_eq!(found_solutions, known_solutions);
    }

    #[test]
    fn test_solve_stupid_corner_case_horizontal() {
        // And while I'm at it, zero size in one dimension...
        let input = "5 0
                     --
                     --





                     "
        .parse::<Input>()
        .unwrap();
        let solver = Solver::from_input(&input);
        let found_solutions = solve_recursive(solver)
            .iter()
            .map(|s| s.to_string())
            .collect::<HashSet<String>>();

        let known_solutions = [""]
            .iter()
            .map(|s| s.to_string())
            .collect::<HashSet<String>>();

        assert_eq!(found_solutions, known_solutions);
    }
}
