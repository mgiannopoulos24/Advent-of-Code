use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

// Function to calculate points for a single card
fn calculate_points(winning_numbers: &[i32], your_numbers: &[i32]) -> i32 {
    let mut matches = 0;
    let mut points = 0;

    for &your_number in your_numbers {
        if winning_numbers.contains(&your_number) {
            matches += 1;
            if matches == 1 {
                points += 1;
            } else {
                points *= 2;
            }
        }
    }

    points
}

// Function to parse a line and return two vectors of winning and your numbers
fn parse_card(line: &str) -> (Vec<i32>, Vec<i32>) {
    let parts: Vec<&str> = line.split('|').collect();
    let winning_numbers: Vec<i32> = parts[0]
        .split_whitespace()
        .filter_map(|num| num.parse().ok()) // Handle invalid integers
        .collect();
    let your_numbers: Vec<i32> = parts[1]
        .split_whitespace()
        .filter_map(|num| num.parse().ok()) // Handle invalid integers
        .collect();

    (winning_numbers, your_numbers)
}

fn main() -> io::Result<()> {
    // Define the input file path
    let path = "input_level_4.txt";

    // Initialize the total points
    let mut total_points = 0;

    // Open the file and read it line by line
    if let Ok(lines) = read_lines(path) {
        for line in lines {
            if let Ok(card) = line {
                let (winning_numbers, your_numbers) = parse_card(&card);
                total_points += calculate_points(&winning_numbers, &your_numbers);
            }
        }
    }

    // Output the total points
    println!("Total points: {}", total_points);

    Ok(())
}

// Helper function to read lines from a file
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
