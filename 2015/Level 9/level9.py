from itertools import permutations

# Maximum number of locations
MAX_LOCATIONS = 10

# Structure to represent distances between two locations
class Distance:
    def __init__(self, src, dest, distance):
        self.src = src
        self.dest = dest
        self.distance = distance

# List to store Distance objects
distances = []
# List to store unique locations
locations = []

# Function to add a unique location
def add_location(location):
    if location not in locations:
        locations.append(location)

# Function to find the distance between two locations
def find_distance(src, dest):
    for distance in distances:
        if (distance.src == src and distance.dest == dest) or (distance.src == dest and distance.dest == src):
            return distance.distance
    return -1  # Return -1 if not found

# Function to calculate the total distance of a route
def calculate_route_distance(route):
    total_distance = 0
    for i in range(len(route) - 1):
        distance = find_distance(route[i], route[i + 1])
        if distance == -1:
            return float('inf')  # Invalid route
        total_distance += distance
    return total_distance

# Function to read and parse the input file
def read_input_file(file_path):
    with open(file_path, 'r') as file:
        for line in file:
            parts = line.strip().split()
            # Expecting the format: "AlphaCentauri to Norrath = 147"
            if len(parts) == 5 and parts[1] == "to" and parts[3] == "=":
                src = parts[0]
                dest = parts[2]
                try:
                    distance = int(parts[4])
                except ValueError:
                    print(f"Invalid distance value in line: {line.strip()}")
                    continue
                add_location(src)
                add_location(dest)
                distances.append(Distance(src, dest, distance))
            else:
                print(f"Skipping malformed line: {line.strip()}")



def main():
    file_path = "input_level_9.txt"  # Make sure this path is correct
    read_input_file(file_path)

    shortest_distance = float('inf')
    for permutation in permutations(locations):
        current_distance = calculate_route_distance(permutation)
        if current_distance < shortest_distance:
            shortest_distance = current_distance

    print(f"The shortest distance is: {shortest_distance}")

if __name__ == "__main__":
    main()
