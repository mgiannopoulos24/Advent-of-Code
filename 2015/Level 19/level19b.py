from collections import deque

def reverse_replacements(replacements):
    # Reverse the direction of the replacements for backward search
    return [(target, source) for source, target in replacements]

def generate_reverse_molecule(replacements, molecule):
    # Generate all possible previous molecules by applying reversed replacements
    prev_molecules = set()
    for source, target in replacements:
        start = 0
        while True:
            start = molecule.find(target, start)
            if start == -1: break
            prev_molecule = molecule[:start] + source + molecule[start+len(target):]
            prev_molecules.add(prev_molecule)
            start += len(target)
    return prev_molecules

def find_shortest_path(replacements, target):
    reversed_replacements = reverse_replacements(replacements)
    queue = deque([(target, 0)])  # molecule, steps
    visited = set()
    
    while queue:
        molecule, steps = queue.popleft()
        if molecule == "e":
            return steps
        if molecule in visited:
            continue
        visited.add(molecule)

        for prev_molecule in generate_reverse_molecule(reversed_replacements, molecule):
            if prev_molecule not in visited:
                queue.append((prev_molecule, steps + 1))

    return -1  # No path found

def main():
    with open('input_level_19.txt', 'r') as f:
        lines = f.readlines()

    replacements = []
    for line in lines[:-2]:
        source, _, target = line.strip().split()
        replacements.append((source, target))

    molecule = lines[-1].strip()

    steps = find_shortest_path(replacements, molecule)
    print(steps)

if __name__ == "__main__":
    main()
