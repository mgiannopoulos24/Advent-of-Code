use std::fs::File;
use std::io::{self, BufRead};
use std::thread;
use std::sync::{Arc, Mutex};

// Struct to hold a mapping range
#[derive(Debug, Clone)]  // Added Clone for the MappingRange struct
struct MappingRange {
    source_start: i64,
    source_end: i64,
    dest_start: i64,
}

// Parse a mapping section and return a sorted vector of tuples representing the mapping ranges
fn parse_mapping_section(lines: &[String]) -> Vec<MappingRange> {
    let mut mapping_ranges = Vec::new();
    
    for line in lines {
        let line = line.trim();
        if line.is_empty() {
            continue; // Skip empty lines
        }

        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() != 3 {
            eprintln!("Invalid mapping line format: '{}'", line);
            continue;
        }

        let dest_start: i64 = match parts[0].parse() {
            Ok(value) => value,
            Err(_) => {
                eprintln!("Non-integer value in mapping line: '{}'", line);
                continue;
            }
        };

        let source_start: i64 = match parts[1].parse() {
            Ok(value) => value,
            Err(_) => {
                eprintln!("Non-integer value in mapping line: '{}'", line);
                continue;
            }
        };

        let range_length: i64 = match parts[2].parse() {
            Ok(value) => value,
            Err(_) => {
                eprintln!("Non-integer value in mapping line: '{}'", line);
                continue;
            }
        };

        if range_length <= 0 {
            eprintln!("Warning: Range length must be positive. Skipping line: '{}'", line);
            continue;
        }

        let source_end = source_start + range_length - 1;
        mapping_ranges.push(MappingRange {
            source_start,
            source_end,
            dest_start,
        });
    }

    // Sort the mapping ranges by source_start
    mapping_ranges.sort_by_key(|x| x.source_start);
    mapping_ranges
}

// Function to build mappings
fn build_mappings(lines: &[String]) -> Vec<Vec<MappingRange>> {
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
    let mut current_section: Option<&str> = None;
    let mut expected_section = section_order.iter();

    for line in lines {
        let lower_line = line.trim().to_lowercase();

        if section_order.iter().any(|&sec| lower_line.starts_with(sec)) {
            if current_section.is_some() && !mapping_lines.is_empty() {
                mappings.push(parse_mapping_section(&mapping_lines));
                mapping_lines.clear();
            }

            if let Some(next_section) = expected_section.next() {
                if lower_line == next_section.to_lowercase() {
                    current_section = Some(next_section);
                } else {
                    eprintln!("Unexpected section header: '{}'.", line);
                    current_section = None;
                }
            }
        } else if current_section.is_some() {
            mapping_lines.push(line.to_string());
        }
    }

    if current_section.is_some() && !mapping_lines.is_empty() {
        mappings.push(parse_mapping_section(&mapping_lines));
    }

    mappings
}

// Function to read the input file and return the seed ranges and mappings
fn get_mappings(file_path: &str) -> (Vec<(i64, i64)>, Vec<Vec<MappingRange>>) {
    let file = File::open(file_path).expect("Unable to open file");
    let reader = io::BufReader::new(file);

    let mut lines: Vec<String> = Vec::new();
    for line in reader.lines() {
        let line = line.expect("Error reading line");
        lines.push(line);
    }

    let mut seed_ranges = Vec::new();
    let mut mapping_start_index = 0;

    for (idx, line) in lines.iter().enumerate() {
        if line.to_lowercase().starts_with("seeds:") {
            let parts: Vec<&str> = line.split(':').collect();
            if parts.len() != 2 {
                eprintln!("Invalid seeds line format: '{}'", line);
                break;
            }

            let seed_values: Vec<&str> = parts[1].trim().split_whitespace().collect();
            if seed_values.len() % 2 != 0 {
                eprintln!("Error: Seeds line has an odd number of values.");
                break;
            }

            let seed_pairs: Vec<i64> = seed_values
                .iter()
                .map(|&x| x.parse::<i64>().unwrap())
                .collect();

            for i in (0..seed_pairs.len()).step_by(2) {
                seed_ranges.push((seed_pairs[i], seed_pairs[i + 1]));
            }

            mapping_start_index = idx + 1;
            break;
        }
    }

    if seed_ranges.is_empty() {
        eprintln!("No valid seed ranges found in the input file.");
        return (seed_ranges, Vec::new());
    }

    let mapping_lines = lines[mapping_start_index..].to_vec();
    let mappings = build_mappings(&mapping_lines);

    (seed_ranges, mappings)
}

// Function to find a mapping number using binary search
fn find_mapping_number(number: i64, mapping_ranges: &[MappingRange]) -> i64 {
    mapping_ranges
        .binary_search_by(|range| {
            if range.source_start <= number && range.source_end >= number {
                std::cmp::Ordering::Equal
            } else if range.source_start > number {
                std::cmp::Ordering::Greater
            } else {
                std::cmp::Ordering::Less
            }
        })
        .map(|idx| {
            let range = &mapping_ranges[idx];
            range.dest_start + (number - range.source_start)
        })
        .unwrap_or(number)
}

// Process a chunk of seeds and return the minimum location number within the chunk
fn process_seeds_chunk(seeds_chunk: &[(i64, i64)], mappings: &[Vec<MappingRange>]) -> i64 {
    seeds_chunk
        .iter()
        .flat_map(|&(start, length)| start..=start + length - 1)
        .map(|seed| {
            mappings.iter().fold(seed, |current_num, mapping| {
                find_mapping_number(current_num, mapping)
            })
        })
        .min()
        .unwrap()
}

// Split seeds into chunks and process them in parallel using threads
fn find_lowest_location_parallel(seed_ranges: &[(i64, i64)], mappings: &[Vec<MappingRange>], chunk_size: usize) -> i64 {
    let seed_chunks: Vec<_> = seed_ranges.chunks(chunk_size).collect();
    let num_threads = seed_chunks.len();  // Dynamic thread count based on seed chunks

    let min_location = Arc::new(Mutex::new(None));

    let handles: Vec<_> = seed_chunks
        .into_iter()
        .map(|chunk| {
            let min_location = Arc::clone(&min_location);
            let chunk = chunk.to_vec();
            let mappings = mappings.to_vec();  // We need Clone to avoid borrowing issues

            thread::spawn(move || {
                let local_min = process_seeds_chunk(&chunk, &mappings);
                let mut global_min = min_location.lock().unwrap();
                match *global_min {
                    Some(current_min) => {
                        if local_min < current_min {
                            *global_min = Some(local_min);
                        }
                    }
                    None => {
                        *global_min = Some(local_min);
                    }
                }
            })
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();
    }

    let final_min_location = min_location.lock().unwrap();
    final_min_location.unwrap()
}

fn main() {
    let input_file = "input_level_5.txt"; // Input file path
    let (seed_ranges, mappings) = get_mappings(input_file);

    if seed_ranges.is_empty() {
        eprintln!("No valid seed ranges to process. Exiting.");
        return;
    }

    if mappings.len() < 7 {
        eprintln!("Error: Insufficient mapping sections. Cannot proceed.");
        return;
    }

    println!(
        "Starting processing of {} seeds using multiple threads...",
        seed_ranges.iter().map(|&(_, length)| length).sum::<i64>()
    );

    let chunk_size = 10_000; // Set chunk size for processing
    let lowest_location = find_lowest_location_parallel(&seed_ranges, &mappings, chunk_size);
    println!("Lowest Location Number: {}", lowest_location);
}

// It needs sometime to run the code, so please be patient. The code is working fine.