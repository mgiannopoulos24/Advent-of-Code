use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Result};

#[derive(Debug)]
struct Game {
    id: usize,
    sets: Vec<HashMap<String, usize>>,
}

fn parse_game(line: &str) -> Option<Game> {
    let parts: Vec<&str> = line.split(": ").collect();
    let game_id: usize = parts[0].split_whitespace().nth(1)?.parse().ok()?;
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

    Some(Game { id: game_id, sets })
}

fn is_game_possible(cube_limits: &HashMap<String, usize>, game: &Game) -> bool {
    for revealed_set in &game.sets {
        for (color, count) in revealed_set {
            if cube_limits.get(color).unwrap_or(&0) < count {
                return false;
            }
        }
    }
    true
}

fn main() -> Result<()> {
    let cube_limits = HashMap::from([
        (String::from("red"), 12),
        (String::from("green"), 13),
        (String::from("blue"), 14),
    ]);

    let file = File::open("input_level_2.txt")?;
    let reader = BufReader::new(file);

    let mut total_sum = 0;

    for line in reader.lines() {
        let line = line?;
        if let Some(game) = parse_game(&line) {
            if is_game_possible(&cube_limits, &game) {
                total_sum += game.id;
            }
        }
    }

    println!("The sum of the IDs of possible games is: {}", total_sum);
    Ok(())
}
