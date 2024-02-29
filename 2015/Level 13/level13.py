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
        total_happiness += happiness[(arrangement[i], left_neighbor)]
        total_happiness += happiness[(arrangement[i], right_neighbor)]
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

def find_all_arrangements():
    global max_happiness, optimal_arrangement
    arrangement = list(range(len(names)))
    # Start permutations from the second element to reduce duplicate efforts due to circular seating
    for perm in itertools.permutations(arrangement[1:]):
        current_arrangement = [arrangement[0]] + list(perm)
        current_happiness = calculate_happiness(current_arrangement)
        if current_happiness > max_happiness:
            max_happiness = current_happiness
            optimal_arrangement = current_arrangement

def main():
    read_input("input_level_13.txt")
    find_all_arrangements()
    print(f"The optimal arrangement has a total happiness of {max_happiness}.")
    print("Optimal seating: ", end="")
    for i in optimal_arrangement:
        print(names[i], end=" ")
    print()

if __name__ == "__main__":
    main()
