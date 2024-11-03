use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

// Function to calculate how many additional cards a card wins
fn calculate_won_cards(cards: &Vec<(Vec<i32>, Vec<i32>)>, idx: usize) -> usize {
    if idx >= cards.len() {
        return 0;
    }

    let (winning_numbers, your_numbers) = &cards[idx];
    let mut matches = 0;

    for &your_number in your_numbers {
        if winning_numbers.contains(&your_number) {
            matches += 1;
        }
    }

    if matches == 0 {
        return 0; // If no matches, no additional cards are won.
    }

    let mut total_won_cards = 0;

    // For each match, process the next set of cards.
    for i in 1..=matches {
        if idx + i < cards.len() {
            total_won_cards += 1 + calculate_won_cards(cards, idx + i);
        }
    }

    total_won_cards
}

// Function to parse a line and return two vectors of winning and your numbers
fn parse_card(line: &str) -> (Vec<i32>, Vec<i32>) {
    let parts: Vec<&str> = line.split('|').collect();
    let winning_numbers: Vec<i32> = parts[0]
        .split_whitespace()
        .filter_map(|num| num.parse().ok())
        .collect();
    let your_numbers: Vec<i32> = parts[1]
        .split_whitespace()
        .filter_map(|num| num.parse().ok())
        .collect();

    (winning_numbers, your_numbers)
}

fn main() -> io::Result<()> {
    // Define the input file path
    let path = "input_level_4.txt";

    // Open the file and read it line by line
    let mut cards = Vec::new();
    if let Ok(lines) = read_lines(path) {
        for line in lines {
            if let Ok(card) = line {
                cards.push(parse_card(&card));
            }
        }
    }

    // Initialize total scratchcards count (starts with original cards)
    let mut total_scratchcards = cards.len();

    // Process each card to see how many additional cards it wins
    for i in 0..cards.len() {
        total_scratchcards += calculate_won_cards(&cards, i);
    }

    // Output the total number of scratchcards (original + won)
    println!("Total scratchcards: {}", total_scratchcards);

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
