use std::fs::File;
use std::io::{self, BufRead, BufReader};

fn main() -> io::Result<()> {
    let file = File::open("input_level_1.txt")?;
    let reader = BufReader::new(file);

    let mut sum = 0;

    for line_result in reader.lines() {
        let line = line_result?;
        let mut first_digit = None;
        let mut last_digit = None;

        for c in line.chars() {
            if c.is_digit(10) {
                let digit = c.to_digit(10).unwrap() as i32;
                if first_digit.is_none() {
                    first_digit = Some(digit);
                }
                last_digit = Some(digit);
            }
        }

        let combined_digit = match (first_digit, last_digit) {
            (Some(fd), Some(ld)) => fd * 10 + ld,
            (Some(fd), None) => fd * 10 + fd,
            _ => 0,
        };

        sum += combined_digit;

        println!("String: {} Combined digit: {}", line, combined_digit);
    }

    println!("Calibration value is: {}", sum);
    Ok(())
}
