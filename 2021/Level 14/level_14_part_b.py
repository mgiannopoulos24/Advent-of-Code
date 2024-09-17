from collections import defaultdict

def read_input(filename):
    with open(filename, 'r') as file:
        lines = [line.strip() for line in file.readlines()]
    
    # The first line is the polymer template
    template = lines[0]
    
    # The rest are the pair insertion rules
    rules = {}
    for line in lines[2:]:  # Skip the first two lines
        pair, insert = line.split(" -> ")
        rules[pair] = insert
    
    return template, rules

def initialize_pair_counts(template):
    pair_counts = defaultdict(int)
    element_counts = defaultdict(int)
    
    # Count the initial pairs
    for i in range(len(template) - 1):
        pair = template[i:i+2]
        pair_counts[pair] += 1
    
    # Count the initial elements
    for char in template:
        element_counts[char] += 1
    
    return pair_counts, element_counts

def apply_insertion_rules(pair_counts, element_counts, rules):
    new_pair_counts = defaultdict(int)
    
    # Apply each rule to the existing pairs
    for pair, count in pair_counts.items():
        if pair in rules:
            insert_element = rules[pair]
            
            # The current pair "AB" will produce two new pairs "A(insert)" and "(insert)B"
            new_pair1 = pair[0] + insert_element
            new_pair2 = insert_element + pair[1]
            
            # Update the new pair counts
            new_pair_counts[new_pair1] += count
            new_pair_counts[new_pair2] += count
            
            # Also, update the count of the inserted element
            element_counts[insert_element] += count
        else:
            # If no rule applies, carry forward the pair count
            new_pair_counts[pair] += count
    
    return new_pair_counts

def simulate_polymer_growth(template, rules, steps):
    # Initialize the pair counts and element counts
    pair_counts, element_counts = initialize_pair_counts(template)
    
    # Perform the insertion steps
    for _ in range(steps):
        pair_counts = apply_insertion_rules(pair_counts, element_counts, rules)
    
    # After the given number of steps, return the element counts
    return element_counts

def calculate_difference(element_counts):
    max_count = max(element_counts.values())
    min_count = min(element_counts.values())
    return max_count - min_count

if __name__ == "__main__":
    # Read input from the file
    template, rules = read_input('input_level_14.txt')
    
    # Simulate the polymer growth for 40 steps
    element_counts = simulate_polymer_growth(template, rules, 40)
    
    # Calculate the difference between the most and least common elements
    result = calculate_difference(element_counts)
    
    print(f"The difference between the most and least common elements is: {result}")
