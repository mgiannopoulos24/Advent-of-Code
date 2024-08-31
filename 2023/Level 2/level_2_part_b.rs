use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Result};

#[derive(Debug)]
struct Game {
    sets: Vec<HashMap<String, usize>>,
}

fn parse_game(line: &str) -> Option<Game> {
    let parts: Vec<&str> = line.split(": ").collect();
    let data = parts[1];

    let sets: Vec<HashMap<String, usize>> = data
        .split("; ")
        .map(|s| {
            let mut map = HashMap::new();
            for item in s.split(", ") {
                let mut parts = item.split_whitespace();
                let count: usize = parts.next()?.parse().ok()?;
                let color = parts.next()?.to_string();
                map.insert(color, count);
            }
            Some(map)
        })
        .collect::<Option<Vec<HashMap<String, usize>>>>()?;

    Some(Game { sets })
}

fn calculate_minimum_cubes(sets: &Vec<HashMap<String, usize>>) -> HashMap<String, usize> {
    let mut min_cubes = HashMap::new();

    for set in sets {
        for (color, count) in set {
            let current_min = min_cubes.entry(color.clone()).or_insert(0);
            if *current_min < *count {
                *current_min = *count;
            }
        }
    }

    min_cubes
}

fn calculate_power(cubes: &HashMap<String, usize>) -> usize {
    let red = cubes.get("red").unwrap_or(&0);
    let green = cubes.get("green").unwrap_or(&0);
    let blue = cubes.get("blue").unwrap_or(&0);

    red * green * blue
}

fn main() -> Result<()> {
    let file = File::open("input_level_2.txt")?;
    let reader = BufReader::new(file);

    let mut total_power = 0;

    for line in reader.lines() {
        let line = line?;
        if let Some(game) = parse_game(&line) {
            let min_cubes = calculate_minimum_cubes(&game.sets);
            let power = calculate_power(&min_cubes);
            total_power += power;
        }
    }

    println!("The sum of the powers of the minimum sets is: {}", total_power);
    Ok(())
}
