def read_input(file_name):
    aunts = {}
    with open(file_name, 'r') as file:
        for line in file:
            parts = line.split(': ', 1)
            sue_number = int(parts[0].split(' ')[1])
            attributes = parts[1].split(', ')
            aunts[sue_number] = {}
            for attribute in attributes:
                key, value = attribute.split(': ')
                aunts[sue_number][key] = int(value)
    return aunts

def find_matching_aunt(aunts, mfcsam_output):
    for sue, attributes in aunts.items():
        match = True
        for key, value in mfcsam_output.items():
            if key in attributes:
                if key == "cats" or key == "trees":
                    if attributes[key] <= value:
                        match = False
                        break
                elif key == "pomeranians" or key == "goldfish":
                    if attributes[key] >= value:
                        match = False
                        break
                else:
                    if attributes[key] != value:
                        match = False
                        break
        if match:
            return sue
    return None

# Path to the input file
input_file = 'input_level_16.txt'
# Read the input file
aunts = read_input(input_file)
# MFCSAM output
mfcsam_output = {
    "children": 3,
    "cats": 7,
    "samoyeds": 2,
    "pomeranians": 3,
    "akitas": 0,
    "vizslas": 0,
    "goldfish": 5,
    "trees": 3,
    "cars": 2,
    "perfumes": 1
}

# Find the matching aunt
matching_aunt = find_matching_aunt(aunts, mfcsam_output)
print(f"The Aunt Sue who gave the gift is: Aunt Sue #{matching_aunt}")
