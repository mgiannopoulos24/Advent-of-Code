use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() -> io::Result<()> {
    // Read the input from a file
    let path = Path::new("input_level_6.txt");
    let file = File::open(&path)?;
    let reader = io::BufReader::new(file);

    let mut times: Vec<usize> = Vec::new();
    let mut distances: Vec<usize> = Vec::new();

    for (index, line) in reader.lines().enumerate() {
        if let Ok(entry) = line {
            if index == 0 {
                // Parse the times from the first line
                times = entry
                    .split_whitespace()
                    .skip(1) // Skip the "Time:" label
                    .map(|x| x.parse::<usize>().unwrap())
                    .collect();
            } else if index == 1 {
                // Parse the distances from the second line
                distances = entry
                    .split_whitespace()
                    .skip(1) // Skip the "Distance:" label
                    .map(|x| x.parse::<usize>().unwrap())
                    .collect();
            }
        }
    }

    if times.len() != distances.len() {
        eprintln!("The number of times and distances don't match.");
        return Ok(());
    }

    let mut total_ways: usize = 1;

    // Process each race
    for (time, record_distance) in times.iter().zip(distances.iter()) {
        let mut ways_to_win = 0;

        // Calculate all possible valid ways to win the race
        for button_hold in 0..=*time {
            let speed = button_hold;
            let travel_time = time - button_hold;
            
            // Ensure travel_time doesn't go negative
            if travel_time > 0 {
                let distance = speed * travel_time;

                if distance > *record_distance {
                    ways_to_win += 1;
                }
            }
        }

        // Multiply the ways to win for each race
        total_ways *= ways_to_win;
    }

    println!("Total ways to win: {}", total_ways);

    Ok(())
}
