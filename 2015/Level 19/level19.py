def generate_molecules(replacements, molecule):
    molecules = set()
    for i in range(len(molecule)):
        for source, target in replacements:
            if molecule[i:i+len(source)] == source:
                new_molecule = molecule[:i] + target + molecule[i+len(source):]
                molecules.add(new_molecule)
    return molecules

def main():
    with open('input_level_19.txt', 'r') as f:
        lines = f.readlines()

    replacements = []
    for line in lines[:-2]:
        source, _, target = line.strip().split()
        replacements.append((source, target))

    molecule = lines[-1].strip()

    distinct_molecules = generate_molecules(replacements, molecule)
    print(len(distinct_molecules))

if __name__ == "__main__":
    main()
