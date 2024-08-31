use std::fs::File;
use std::io::{self, BufRead};

fn is_digit(c: char) -> bool {
    c.is_digit(10)
}

fn main() -> io::Result<()> {
    let path = "input_level_1.txt";
    let file = File::open(&path)?;
    let reader = io::BufReader::new(file);

    let nums = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];
    let mut sum = 0;

    for line in reader.lines() {
        let line = line?;
        let mut first_digit = None;
        let mut last_digit = None;

        let mut i = 0;
        while i < line.len() {
            let c = line.chars().nth(i).unwrap();
            if is_digit(c) {
                let digit = c.to_digit(10).unwrap();
                if first_digit.is_none() {
                    first_digit = Some(digit);
                } else {
                    last_digit = Some(digit);
                }
            } else {
                let remaining = &line[i..];
                for (j, &num) in nums.iter().enumerate() {
                    if remaining.starts_with(num) {
                        let n = (j + 1) as u32;
                        if first_digit.is_none() {
                            first_digit = Some(n);
                        } else {
                            last_digit = Some(n);
                        }
                        i += num.len() - 1; 
                        break;
                    }
                }
            }
            i += 1;
        }

        let first_digit = first_digit.unwrap_or(0);
        let last_digit = last_digit.unwrap_or(first_digit); 
        sum += first_digit * 10 + last_digit;
    }

    println!("Calibration value: {}", sum);

    Ok(())
}
