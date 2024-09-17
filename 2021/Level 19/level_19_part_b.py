from itertools import combinations

def solve(puzzle_input):
    class Scanner:
        def __init__(self, beacons):
            self.beacons = beacons
            self.final = set()

        def xy_rotations(self):
            yield lambda x, y, z: (x, y, z)
            yield lambda x, y, z: (y, -x, z)
            yield lambda x, y, z: (-x, -y, z)
            yield lambda x, y, z: (-y, x, z)

        def yz_rotations(self):
            yield lambda x, y, z: (x, y, z)
            yield lambda x, y, z: (x, z, -y)
            yield lambda x, y, z: (x, -y, -z)
            yield lambda x, y, z: (x, -z, y)

        def xz_rotations(self):
            yield lambda x, y, z: (x, y, z)
            yield lambda x, y, z: (z, y, -x)
            yield lambda x, y, z: (-x, y, -z)
            yield lambda x, y, z: (-z, y, x)

        def rotations(self):
            for xy in self.xy_rotations():
                for yz in self.yz_rotations():
                    for xz in self.xz_rotations():
                        rotated_beacons = set()
                        for beacon in self.beacons:
                            rotated_beacons.add(xz(*yz(*xy(*beacon))))
                        yield rotated_beacons

        def translate(self, beacons, offset):
            return set([(beacon[0] + offset[0], beacon[1] + offset[1], beacon[2] + offset[2]) for beacon in beacons])

    scanners = []
    beacons = set()
    for line in puzzle_input:
        if line.startswith("---"):
            continue
        elif line == "":
            scanners.append(Scanner(beacons))
            beacons = set()
        else:
            points = [int(n) for n in line.split(",")]
            beacons.add(tuple(points))
    scanners.append(Scanner(beacons))
    
    fixed = set()
    fixed.add(scanners[0])
    scanners[0].final = scanners[0].beacons
    scanners[0].offset = (0, 0, 0)

    while len(fixed) != len(scanners):
        for scanner in scanners:
            if scanner in fixed:
                continue

            if len(fixed) == 1:
                fixed_beacons = scanners[0].final
            else:
                fixed_beacons = scanners[0].final.union(*[s.final for s in fixed if s is not scanners[0]])
            for r in scanner.rotations():
                for fb in fixed_beacons:
                    for sb in r:
                        offset = (fb[0] - sb[0], fb[1] - sb[1], fb[2] - sb[2])
                        shifted = scanner.translate(r, offset)
                        if len(shifted & fixed_beacons) >= 12:
                            scanner.final = shifted
                            scanner.offset = offset
                            fixed.add(scanner)
                            break

    def manhattan_distance(p1, p2):
        return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1]) + abs(p1[2] - p2[2])

    return max(manhattan_distance(a, b) for a, b in combinations([s.offset for s in scanners], 2))

def main():
    with open("input_level_19.txt") as f:
        puzzle_input = f.read().splitlines()

    distance = solve(puzzle_input)

    print("The largest Manhattan distance between any two scanners is " + str(distance) + ".")

if __name__ == "__main__":
    main()
