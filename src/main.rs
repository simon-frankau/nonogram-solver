// Nonogram solver
//
// Copyright 2021 Simon Frankau
//

#[cfg(test)]
use std::collections::HashSet;

// Generate a text representation of a bitmap.
//
// n is the number of bits actually being used.
fn bitmap_to_string(bitmap: u64, n: usize) -> String {
    let mut acc = String::new();
    for i in 1..=n {
        acc.push(if (bitmap >> (n - i)) & 1 != 0 { 'X' } else { '.' });
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
    assert!(n <= 64);

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
    fn expand_aux(
        counts: &[usize],
        num_blanks: usize,
        so_far: u64,
        accumulator: &mut Vec<u64>,
    ) {
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

fn main() {
    let n = 5;
    let results = expand(&vec![1, 2], n);
    for result in results.iter() {
        println!("{}", bitmap_to_string(*result, n));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

        let expected_set = expected
            .iter()
            .cloned()
            .collect::<HashSet<&str>>();

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
            "X.XX.XXX.......", "X.XX..XXX......", "X.XX...XXX.....",
            "X.XX....XXX....", "X.XX.....XXX...", "X.XX......XXX..",
            "X.XX.......XXX.", "X.XX........XXX", "X..XX.XXX......",
            "X..XX..XXX.....", "X..XX...XXX....", "X..XX....XXX...",
            "X..XX.....XXX..", "X..XX......XXX.", "X..XX.......XXX",
            "X...XX.XXX.....", "X...XX..XXX....", "X...XX...XXX...",
            "X...XX....XXX..", "X...XX.....XXX.", "X...XX......XXX",
            "X....XX.XXX....", "X....XX..XXX...", "X....XX...XXX..",
            "X....XX....XXX.", "X....XX.....XXX", "X.....XX.XXX...",
            "X.....XX..XXX..", "X.....XX...XXX.", "X.....XX....XXX",
            "X......XX.XXX..", "X......XX..XXX.", "X......XX...XXX",
            "X.......XX.XXX.", "X.......XX..XXX", "X........XX.XXX",
            ".X.XX.XXX......", ".X.XX..XXX.....", ".X.XX...XXX....",
            ".X.XX....XXX...", ".X.XX.....XXX..", ".X.XX......XXX.",
            ".X.XX.......XXX", ".X..XX.XXX.....", ".X..XX..XXX....",
            ".X..XX...XXX...", ".X..XX....XXX..", ".X..XX.....XXX.",
            ".X..XX......XXX", ".X...XX.XXX....", ".X...XX..XXX...",
            ".X...XX...XXX..", ".X...XX....XXX.", ".X...XX.....XXX",
            ".X....XX.XXX...", ".X....XX..XXX..", ".X....XX...XXX.",
            ".X....XX....XXX", ".X.....XX.XXX..", ".X.....XX..XXX.",
            ".X.....XX...XXX", ".X......XX.XXX.", ".X......XX..XXX",
            ".X.......XX.XXX", "..X.XX.XXX.....", "..X.XX..XXX....",
            "..X.XX...XXX...", "..X.XX....XXX..", "..X.XX.....XXX.",
            "..X.XX......XXX", "..X..XX.XXX....", "..X..XX..XXX...",
            "..X..XX...XXX..", "..X..XX....XXX.", "..X..XX.....XXX",
            "..X...XX.XXX...", "..X...XX..XXX..", "..X...XX...XXX.",
            "..X...XX....XXX", "..X....XX.XXX..", "..X....XX..XXX.",
            "..X....XX...XXX", "..X.....XX.XXX.", "..X.....XX..XXX",
            "..X......XX.XXX", "...X.XX.XXX....", "...X.XX..XXX...",
            "...X.XX...XXX..", "...X.XX....XXX.", "...X.XX.....XXX",
            "...X..XX.XXX...", "...X..XX..XXX..", "...X..XX...XXX.",
            "...X..XX....XXX", "...X...XX.XXX..", "...X...XX..XXX.",
            "...X...XX...XXX", "...X....XX.XXX.", "...X....XX..XXX",
            "...X.....XX.XXX", "....X.XX.XXX...", "....X.XX..XXX..",
            "....X.XX...XXX.", "....X.XX....XXX", "....X..XX.XXX..",
            "....X..XX..XXX.", "....X..XX...XXX", "....X...XX.XXX.",
            "....X...XX..XXX", "....X....XX.XXX", ".....X.XX.XXX..",
            ".....X.XX..XXX.", ".....X.XX...XXX", ".....X..XX.XXX.",
            ".....X..XX..XXX", ".....X...XX.XXX", "......X.XX.XXX.",
            "......X.XX..XXX", "......X..XX.XXX", ".......X.XX.XXX"
        ];

        assert!(is_equiv(&bitmaps, n, &expected));
    }

    #[test]
    fn test_expand_large() {
        let n = 40;
        let bitmaps = expand(&vec![1, 2, 3, 4, 5], n);
        assert_eq!(bitmaps.len(), 65780);
    }
}
