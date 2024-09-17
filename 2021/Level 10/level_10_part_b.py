def read_input(filename):
    with open(filename, 'r') as file:
        lines = [line.strip() for line in file.readlines()]
    return lines

def is_corrupted_or_incomplete(line):
    # Define the matching pairs for chunk characters
    matching_pairs = {')': '(', ']': '[', '}': '{', '>': '<'}
    
    stack = []
    
    for char in line:
        if char in "([{<":
            stack.append(char)
        elif char in ")]}>":
            if stack and stack[-1] == matching_pairs[char]:
                stack.pop()
            else:
                return 'corrupted', None  # Corrupted line
    
    # If there's anything left in the stack, it's incomplete
    return 'incomplete', stack if stack else None

def complete_line(opening_stack):
    # Find the completion string that closes the remaining open characters
    closing_pairs = {'(': ')', '[': ']', '{': '}', '<': '>'}
    completion_string = []
    
    while opening_stack:
        completion_string.append(closing_pairs[opening_stack.pop()])
    
    return completion_string

def score_completion_string(completion_string):
    # Scoring system
    score_table = {')': 1, ']': 2, '}': 3, '>': 4}
    
    total_score = 0
    for char in completion_string:
        total_score = total_score * 5 + score_table[char]
    
    return total_score

def calculate_middle_score(lines):
    completion_scores = []
    
    for line in lines:
        result, stack = is_corrupted_or_incomplete(line)
        if result == 'incomplete':
            # Get the completion string
            completion_string = complete_line(stack)
            # Score the completion string
            score = score_completion_string(completion_string)
            completion_scores.append(score)
    
    # Sort the scores and find the middle one
    completion_scores.sort()
    middle_index = len(completion_scores) // 2
    return completion_scores[middle_index]

if __name__ == "__main__":
    # Read input from the file
    lines = read_input('input_level_10.txt')
    
    # Calculate the middle score of completion strings
    middle_score = calculate_middle_score(lines)
    
    print(f"The middle score is: {middle_score}")
