import itertools

MAX_PEOPLE = 10
MAX_NAME_LENGTH = 50

happiness = {}
names = []
max_happiness = float('-inf')
optimal_arrangement = []

def find_name_index(name):
    if name not in names:
        names.append(name)
    return names.index(name)

def calculate_happiness(arrangement):
    total_happiness = 0
    num_people = len(arrangement)
    for i in range(num_people):
        left_neighbor = arrangement[(i + num_people - 1) % num_people]
        right_neighbor = arrangement[(i + 1) % num_people]
        total_happiness += happiness.get((arrangement[i], left_neighbor), 0)
        total_happiness += happiness.get((arrangement[i], right_neighbor), 0)
    return total_happiness

def read_input(filename):
    with open(filename, 'r') as file:
        for line in file:
            parts = line.strip().split()
            person1, verb, happiness_units, person2 = parts[0], parts[2], int(parts[3]), parts[-1][:-1]
            if verb == "lose":
                happiness_units = -happiness_units
            index1 = find_name_index(person1)
            index2 = find_name_index(person2)
            happiness[(index1, index2)] = happiness_units

def add_yourself():
    my_index = find_name_index("You")
    # Assuming a neutral happiness score with everyone and vice versa
    for name in names:
        if name != "You":
            index = find_name_index(name)
            happiness[(my_index, index)] = 0
            happiness[(index, my_index)] = 0

def find_all_arrangements():
    global max_happiness, optimal_arrangement
    arrangement = list(range(len(names)))
    for perm in itertools.permutations(arrangement):
        current_happiness = calculate_happiness(perm)
        if current_happiness > max_happiness:
            max_happiness = current_happiness
            optimal_arrangement = perm

def main():
    read_input("input_level_13.txt")
    add_yourself()  # Add yourself to the list and adjust happiness scores
    find_all_arrangements()
    print(f"The optimal arrangement, including yourself, has a total happiness of {max_happiness}.")
    print("Optimal seating: ", end="")
    for i in optimal_arrangement:
        print(names[i], end=" ")
    print()

if __name__ == "__main__":
    main()
