use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() -> io::Result<()> {
    // Read the input from a file
    let path = Path::new("input_level_6.txt");
    let file = File::open(&path)?;
    let reader = io::BufReader::new(file);

    let mut time_str = String::new();
    let mut distance_str = String::new();

    // Process the input file to read the concatenated numbers
    for (index, line) in reader.lines().enumerate() {
        if let Ok(entry) = line {
            if index == 0 {
                // Concatenate all the numbers in the time line into one large number
                time_str = entry.split_whitespace().skip(1).collect();
            } else if index == 1 {
                // Concatenate all the numbers in the distance line into one large number
                distance_str = entry.split_whitespace().skip(1).collect();
            }
        }
    }

    // Parse the concatenated time and distance into a single number
    let total_time: usize = time_str.parse().unwrap();
    let record_distance: usize = distance_str.parse().unwrap();

    let mut ways_to_win = 0;

    // Calculate all possible valid ways to win the race
    for button_hold in 0..=total_time {
        let speed = button_hold;
        let travel_time = total_time - button_hold;

        if travel_time > 0 {
            let distance = speed * travel_time;

            if distance > record_distance {
                ways_to_win += 1;
            }
        }
    }

    println!("Total ways to win: {}", ways_to_win);

    Ok(())
}
