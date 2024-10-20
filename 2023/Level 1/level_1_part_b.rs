use std::fs::File;
use std::io::{self, BufRead, BufReader};

fn main() -> io::Result<()> {
    let file = File::open("input_level_1.txt")?;
    let reader = BufReader::new(file);

    let mut sum = 0;

    // List of spelled-out digits and their numeric equivalents
    let digit_words = vec![
        ("one", 1),
        ("two", 2),
        ("three", 3),
        ("four", 4),
        ("five", 5),
        ("six", 6),
        ("seven", 7),
        ("eight", 8),
        ("nine", 9),
    ];

    for line_result in reader.lines() {
        let line = line_result?;
        let line_lower = line.to_lowercase();
        let n = line_lower.len();
        let chars: Vec<char> = line_lower.chars().collect();

        let mut digits = Vec::new();

        for i in 0..n {
            let c = chars[i];

            // Check for numeric digits
            if c.is_digit(10) {
                let digit = c.to_digit(10).unwrap() as i32;
                digits.push((i, digit));
                continue;
            }

            // Check for spelled-out digits
            for (word, value) in &digit_words {
                let word_len = word.len();
                if i + word_len <= n {
                    let slice: String = chars[i..i + word_len].iter().collect();
                    if slice == *word {
                        digits.push((i, *value));
                        // Do not advance i; check for overlapping matches
                        break;
                    }
                }
            }
        }

        if !digits.is_empty() {
            digits.sort_by_key(|&(pos, _)| pos);
            let first_digit = digits.first().unwrap().1;
            let last_digit = digits.last().unwrap().1;
            let combined_digit = first_digit * 10 + last_digit;
            sum += combined_digit;
            println!("String: {} Combined digit: {}", line, combined_digit);
        } else {
            println!("String: {} Combined digit: 0", line);
        }
    }

    println!("Calibration value is: {}", sum);
    Ok(())
}
