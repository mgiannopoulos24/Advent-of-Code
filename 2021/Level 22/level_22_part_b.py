class Cuboid:
    def __init__(self, x_min, x_max, y_min, y_max, z_min, z_max):
        self.x_min = x_min
        self.x_max = x_max
        self.y_min = y_min
        self.y_max = y_max
        self.z_min = z_min
        self.z_max = z_max

    def volume(self):
        return (self.x_max - self.x_min + 1) * (self.y_max - self.y_min + 1) * (self.z_max - self.z_min + 1)

    def intersect(self, other):
        x_min = max(self.x_min, other.x_min)
        x_max = min(self.x_max, other.x_max)
        y_min = max(self.y_min, other.y_min)
        y_max = min(self.y_max, other.y_max)
        z_min = max(self.z_min, other.z_min)
        z_max = min(self.z_max, other.z_max)

        if x_min <= x_max and y_min <= y_max and z_min <= z_max:
            return Cuboid(x_min, x_max, y_min, y_max, z_min, z_max)
        else:
            return None  # No intersection

def parse_input(file_name):
    steps = []
    with open(file_name, 'r') as file:
        for line in file:
            action, ranges = line.strip().split(" ")
            x_range, y_range, z_range = ranges.split(',')
            x_min, x_max = map(int, x_range[2:].split(".."))
            y_min, y_max = map(int, y_range[2:].split(".."))
            z_min, z_max = map(int, z_range[2:].split(".."))
            steps.append((action == "on", Cuboid(x_min, x_max, y_min, y_max, z_min, z_max)))
    return steps

def reboot_reactor(steps):
    active_cuboids = []

    for on, cuboid in steps:
        new_cuboids = []

        # Subtract cuboid intersections from existing active cuboids
        for is_on, active_cuboid in active_cuboids:
            intersection = active_cuboid.intersect(cuboid)
            if intersection:
                # If there's an intersection, invert the state of the cuboid to subtract/add correctly
                new_cuboids.append((not is_on, intersection))

        # If the current cuboid is an "on" cuboid, add it to the list
        if on:
            new_cuboids.append((True, cuboid))

        # Update the active cuboids with the new cuboids (add both positive and negative regions)
        active_cuboids.extend(new_cuboids)

    # Calculate the total volume of all active cuboids
    total_volume = 0
    for is_on, cuboid in active_cuboids:
        volume = cuboid.volume()
        total_volume += volume if is_on else -volume

    return total_volume



def main(file_name):
    steps = parse_input(file_name)
    result = reboot_reactor(steps)
    print(f"Number of cubes that are on: {result}")

# Example usage:
if __name__ == "__main__":
    main("input_level_22.txt")
