import numpy as np
from collections import defaultdict

# Function to parse the input file and return a list of scanner's beacon positions
def parse_input(file_name):
    scanners = []
    with open(file_name) as file:
        scanner = []
        for line in file:
            line = line.strip()
            if line.startswith("--- scanner"):
                if scanner:
                    scanners.append(scanner)
                    scanner = []
            elif line:
                scanner.append(tuple(map(int, line.split(','))))
        if scanner:
            scanners.append(scanner)
    return scanners

# Function to generate all 24 possible rotations of a given beacon
def generate_rotations(beacon):
    x, y, z = beacon
    return [
        (x, y, z), (x, -z, y), (x, -y, -z), (x, z, -y),
        (-x, -y, z), (-x, z, y), (-x, y, -z), (-x, -z, -y),
        (y, z, x), (y, -x, z), (y, -z, -x), (y, x, -z),
        (-y, -z, x), (-y, x, z), (-y, z, -x), (-y, -x, -z),
        (z, x, y), (z, -y, x), (z, -x, -y), (z, y, -x),
        (-z, -x, y), (-z, y, x), (-z, x, -y), (-z, -y, -x)
    ]

# Function to find matching beacons between two scanners (at least 12 matches)
def find_match(scanner1, scanner2):
    for rotation in range(24):
        rotated_scanner2 = [generate_rotations(beacon)[rotation] for beacon in scanner2]
        diffs = defaultdict(int)
        for b1 in scanner1:
            for b2 in rotated_scanner2:
                diff = tuple(np.subtract(b1, b2))
                diffs[diff] += 1
                if diffs[diff] >= 12:
                    return diff, rotated_scanner2
    return None, None

# Main function to assemble the map and count unique beacons
def assemble_map(scanners):
    scanner_positions = {0: (0, 0, 0)}
    aligned_scanners = {0: scanners[0]}
    unaligned_scanners = set(range(1, len(scanners)))

    while unaligned_scanners:
        for aligned in list(aligned_scanners.keys()):
            for unaligned in list(unaligned_scanners):
                offset, rotated = find_match(aligned_scanners[aligned], scanners[unaligned])
                if offset:
                    scanner_positions[unaligned] = tuple(np.add(scanner_positions[aligned], offset))
                    aligned_scanners[unaligned] = [tuple(np.add(beacon, offset)) for beacon in rotated]
                    unaligned_scanners.remove(unaligned)
                    break

    # Collect all unique beacons
    unique_beacons = set()
    for beacons in aligned_scanners.values():
        unique_beacons.update(beacons)

    return len(unique_beacons), scanner_positions

# Load the scanner data and assemble the beacon map
def main(file_name):
    scanners = parse_input(file_name)
    unique_beacon_count, scanner_positions = assemble_map(scanners)
    print(f"Number of unique beacons: {unique_beacon_count}")
    return unique_beacon_count, scanner_positions

# Example usage:
if __name__ == "__main__":
    main("input_level_19.txt")
