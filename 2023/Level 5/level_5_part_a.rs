use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

// Struct to hold a mapping range
#[derive(Debug)]
struct MappingRange {
    source_start: i64,
    source_end: i64,
    dest_start: i64,
}

// Function to parse a mapping section and return a vector of tuples representing the mapping ranges
fn parse_mapping_section(lines: Vec<String>) -> Vec<MappingRange> {
    let mut mapping_ranges = Vec::new();
    
    for line in lines {
        let line = line.trim();
        if line.is_empty() {
            continue; // Skip empty lines
        }
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() != 3 {
            println!("Invalid mapping line format: '{}'", line);
            continue;
        }

        let dest_start: i64 = match parts[0].parse() {
            Ok(value) => value,
            Err(_) => {
                println!("Non-integer value in mapping line: '{}'", line);
                continue;
            }
        };

        let source_start: i64 = match parts[1].parse() {
            Ok(value) => value,
            Err(_) => {
                println!("Non-integer value in mapping line: '{}'", line);
                continue;
            }
        };

        let range_length: i64 = match parts[2].parse() {
            Ok(value) => value,
            Err(_) => {
                println!("Non-integer value in mapping line: '{}'", line);
                continue;
            }
        };

        let source_end = match source_start.checked_add(range_length - 1) {
            Some(end) => end,
            None => {
                println!("Overflow when calculating source_end in line: '{}'", line);
                continue;
            }
        };

        mapping_ranges.push(MappingRange {
            source_start,
            source_end,
            dest_start,
        });
    }

    mapping_ranges
}

// Function to build mappings
fn build_mappings(lines: Vec<String>) -> Vec<Vec<MappingRange>> {
    let mut mappings = Vec::new();
    let section_order = vec![
        "seed-to-soil map:",
        "soil-to-fertilizer map:",
        "fertilizer-to-water map:",
        "water-to-light map:",
        "light-to-temperature map:",
        "temperature-to-humidity map:",
        "humidity-to-location map:",
    ];
    let mut mapping_lines = Vec::new();
    let mut current_section = None;
    let mut expected_section = section_order.iter();

    for line in lines {
        let line = line.trim().to_lowercase();

        if section_order.iter().any(|&sec| line.starts_with(sec)) {
            if let Some(cs) = current_section {
                if !mapping_lines.is_empty() {
                    mappings.push(parse_mapping_section(mapping_lines.clone()));
                    mapping_lines.clear();
                }
            }

            if let Some(next_section) = expected_section.next() {
                if line == next_section.to_lowercase() {
                    current_section = Some(next_section);
                } else {
                    println!("Unexpected section header: '{}'.", line);
                    current_section = None;
                }
            }
        } else if current_section.is_some() {
            mapping_lines.push(line.clone());
        }
    }

    if current_section.is_some() && !mapping_lines.is_empty() {
        mappings.push(parse_mapping_section(mapping_lines));
    }

    mappings
}

// Function to read the input file and return the seeds and mappings
fn get_mappings(file_path: &str) -> (Vec<i64>, Vec<Vec<MappingRange>>) {
    let file = File::open(file_path).expect("Unable to open file");
    let reader = io::BufReader::new(file);

    let mut seeds = Vec::new();
    let mut lines: Vec<String> = Vec::new();

    for line in reader.lines() {
        let line = line.expect("Error reading line");
        lines.push(line);
    }

    let mut mapping_start_index = 0;

    for (idx, line) in lines.iter().enumerate() {
        if line.to_lowercase().starts_with("seeds:") {
            let parts: Vec<&str> = line.split(':').collect();
            if parts.len() != 2 {
                println!("Invalid seeds line format: '{}'", line);
                break;
            }
            let seed_numbers: Vec<&str> = parts[1].trim().split_whitespace().collect();
            for seed in seed_numbers {
                match seed.parse::<i64>() {
                    Ok(value) => seeds.push(value),
                    Err(_) => {
                        println!("Non-integer seed numbers in line: '{}'", line);
                        break;
                    }
                }
            }
            mapping_start_index = idx + 1;
            break;
        }
    }

    if seeds.is_empty() {
        println!("No seeds found in the input file.");
        return (seeds, Vec::new());
    }

    let mapping_lines = lines[mapping_start_index..].to_vec();
    let mappings = build_mappings(mapping_lines);

    (seeds, mappings)
}

// Function to apply a mapping to a number
fn apply_mapping(number: i64, mapping_ranges: &Vec<MappingRange>) -> i64 {
    for range in mapping_ranges {
        if number >= range.source_start && number <= range.source_end {
            let offset = number - range.source_start;
            return range.dest_start + offset;
        }
    }
    number // Default mapping
}

// Function to find the lowest location
fn find_lowest_location(seeds: Vec<i64>, mappings: Vec<Vec<MappingRange>>) -> Option<i64> {
    let mut location_numbers = Vec::new();

    for seed in seeds {
        let mut current_num = seed;
        for mapping in &mappings {
            current_num = apply_mapping(current_num, mapping);
        }
        location_numbers.push(current_num);
    }

    if location_numbers.is_empty() {
        println!("No location numbers derived from seeds.");
        return None;
    }

    Some(*location_numbers.iter().min().unwrap())
}

fn main() {
    let input_file = "input_level_5.txt"; // Input file path
    let (seeds, mappings) = get_mappings(input_file);
    if seeds.is_empty() {
        println!("No seeds to process.");
        return;
    }

    if mappings.len() < 7 {
        println!("Insufficient mapping sections. Cannot proceed.");
        return;
    }

    match find_lowest_location(seeds, mappings) {
        Some(lowest_location) => println!("Lowest Location Number: {}", lowest_location),
        None => println!("Could not determine the lowest location number."),
    }
}
