from collections import deque

def generate_molecules(replacements, molecule):
    molecules = set()
    for i in range(len(molecule)):
        for source, target in replacements:
            if molecule[i:i+len(source)] == source:
                new_molecule = molecule[:i] + target + molecule[i+len(source):]
                molecules.add(new_molecule)
    return molecules

def find_steps_to_medicine(replacements, medicine):
    queue = deque([(medicine, 0)])  # (molecule, steps)
    visited = set()

    while queue:
        current_molecule, steps = queue.popleft()

        if current_molecule == 'e':
            return steps

        for source, target in replacements:
            for i in range(len(current_molecule)):
                if current_molecule[i:i+len(target)] == target:
                    new_molecule = current_molecule[:i] + source + current_molecule[i+len(target):]

                    if new_molecule not in visited:
                        visited.add(new_molecule)
                        queue.append((new_molecule, steps + 1))

    return -1  # No solution found

def main():
    with open('input_level_19.txt', 'r') as f:
        lines = f.readlines()

    replacements = []
    for line in lines[:-2]:
        source, _, target = line.strip().split()
        replacements.append((target, source))  # Reversing source and target

    medicine = lines[-1].strip()

    steps = find_steps_to_medicine(replacements, medicine)
    print("Fewest number of steps to go from 'e' to the medicine molecule:", steps)

if __name__ == "__main__":
    main()
