import math
import json
from copy import deepcopy

# Function to add two snailfish numbers
def add(left, right):
    return [deepcopy(left), deepcopy(right)]

# Function to reduce the snailfish number
def reduce_snailfish(snailfish):
    while True:
        exploded, snailfish, _, _ = explode(snailfish)
        if exploded:
            continue
        split_occurred, snailfish = split_number(snailfish)
        if split_occurred:
            continue
        break
    return snailfish

# Function to handle the exploding process
def explode(snailfish, depth=0):
    if isinstance(snailfish, int):
        return False, snailfish, None, None
    
    if depth == 4:
        return True, 0, snailfish[0], snailfish[1]
    
    # Try to explode the left side
    exploded, left, left_val, right_val = explode(snailfish[0], depth + 1)
    if exploded:
        if right_val is not None:
            snailfish[1] = add_to_leftmost(snailfish[1], right_val)
        return True, [left, snailfish[1]], left_val, None
    
    # Try to explode the right side
    exploded, right, left_val, right_val = explode(snailfish[1], depth + 1)
    if exploded:
        if left_val is not None:
            snailfish[0] = add_to_rightmost(snailfish[0], left_val)
        return True, [snailfish[0], right], None, right_val
    
    return False, snailfish, None, None

# Helper function to add value to the leftmost number
def add_to_leftmost(snailfish, value):
    if isinstance(snailfish, int):
        return snailfish + value
    left, right = snailfish
    return [add_to_leftmost(left, value), right]

# Helper function to add value to the rightmost number
def add_to_rightmost(snailfish, value):
    if isinstance(snailfish, int):
        return snailfish + value
    left, right = snailfish
    return [left, add_to_rightmost(right, value)]

# Function to handle the splitting process
def split_number(snailfish):
    if isinstance(snailfish, int):
        if snailfish >= 10:
            return True, [snailfish // 2, math.ceil(snailfish / 2)]
        else:
            return False, snailfish
    
    # Try splitting the left side
    split_occurred, left = split_number(snailfish[0])
    if split_occurred:
        return True, [left, snailfish[1]]
    
    # Try splitting the right side
    split_occurred, right = split_number(snailfish[1])
    if split_occurred:
        return True, [snailfish[0], right]
    
    return False, snailfish

# Function to calculate the magnitude of a snailfish number
def magnitude(snailfish):
    if isinstance(snailfish, int):
        return snailfish
    left, right = snailfish
    return 3 * magnitude(left) + 2 * magnitude(right)

# Function to find the largest magnitude from adding any two snailfish numbers
def find_largest_magnitude(snailfish_numbers):
    max_magnitude = 0

    # Try all pairs of distinct snailfish numbers
    for i in range(len(snailfish_numbers)):
        for j in range(len(snailfish_numbers)):
            if i != j:
                # Add snailfish_numbers[i] + snailfish_numbers[j]
                result = add(snailfish_numbers[i], snailfish_numbers[j])
                reduced_result = reduce_snailfish(result)
                current_magnitude = magnitude(reduced_result)
                
                # Update the max magnitude
                max_magnitude = max(max_magnitude, current_magnitude)

    return max_magnitude

if __name__ == "__main__":
    # Read input from the file
    with open("input_level_18.txt") as file:
        snailfish_numbers = [json.loads(line.strip()) for line in file.readlines()]
    
    # Find the largest magnitude from adding any two snailfish numbers
    largest_magnitude = find_largest_magnitude(snailfish_numbers)
    
    print(f"The largest magnitude of any sum of two different snailfish numbers is: {largest_magnitude}")
