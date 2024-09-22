use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

/// Determines if a character is a symbol.
/// Symbols are any characters that are not digits and not periods.
fn is_symbol(c: char) -> bool {
    !c.is_digit(10) && c != '.'
}

/// Generates all valid adjacent positions (including diagonals) for a given cell.
fn get_adjacent_positions(row: usize, col: usize, max_rows: usize, max_cols: usize) -> Vec<(usize, usize)> {
    let mut positions = Vec::new();
    // Define the relative directions (8 neighbors)
    let directions = [
        (-1isize, -1isize),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ];

    for &(dr, dc) in &directions {
        let new_row = row as isize + dr;
        let new_col = col as isize + dc;

        if new_row >= 0
            && new_row < max_rows as isize
            && new_col >= 0
            && new_col < max_cols as isize
        {
            positions.push((new_row as usize, new_col as usize));
        }
    }

    positions
}

fn main() -> io::Result<()> {
    // Path to the input file
    let path = Path::new("input_level_3.txt");

    // Open the file in read-only mode (ignoring errors).
    let file = File::open(&path)?;
    let reader = io::BufReader::new(file);

    // Read the schematic into a 2D grid (vector of vectors)
    let grid: Vec<Vec<char>> = reader
        .lines()
        .map(|line| line.unwrap().chars().collect())
        .collect();

    let max_rows = grid.len();
    let max_cols = grid.iter().map(|row| row.len()).max().unwrap_or(0);

    // Vector to store numbers with their positions and values
    struct Number {
        row: usize,
        start_col: usize,
        end_col: usize,
        value: u64,
    }

    let mut numbers: Vec<Number> = Vec::new();

    // Identify all numbers in the grid along with their positions
    for (row_idx, row) in grid.iter().enumerate() {
        let mut col = 0;
        while col < row.len() {
            if row[col].is_digit(10) {
                let start_col = col;
                let mut num_str = String::new();
                num_str.push(row[col]);
                col += 1;
                // Continue to capture multi-digit numbers
                while col < row.len() && row[col].is_digit(10) {
                    num_str.push(row[col]);
                    col += 1;
                }
                let end_col = col - 1;
                let number = Number {
                    row: row_idx,
                    start_col,
                    end_col,
                    value: num_str.parse::<u64>().unwrap(),
                };
                numbers.push(number);
            } else {
                col += 1;
            }
        }
    }

    let mut part_numbers: Vec<u64> = Vec::new();

    // Check each number for adjacency to symbols
    'number_loop: for num in &numbers {
        for col in num.start_col..=num.end_col {
            // Ensure the column is within the row's length
            if col >= grid[num.row].len() {
                continue;
            }
            for (adj_row, adj_col) in get_adjacent_positions(num.row, col, max_rows, max_cols) {
                // Ensure the adjacent column is within the adjacent row's length
                if adj_col >= grid[adj_row].len() {
                    continue;
                }
                let c = grid[adj_row][adj_col];
                if is_symbol(c) {
                    part_numbers.push(num.value);
                    continue 'number_loop; // Move to the next number
                }
            }
        }
    }

    // Calculate the total sum of part numbers
    let total: u64 = part_numbers.iter().sum();
    println!("{}", total);

    Ok(())
}
