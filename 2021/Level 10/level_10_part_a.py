def read_input(filename):
    with open(filename, 'r') as file:
        lines = [line.strip() for line in file.readlines()]
    return lines

def calculate_syntax_error_score(lines):
    # Define the matching pairs for chunk characters
    matching_pairs = {')': '(', ']': '[', '}': '{', '>': '<'}
    
    # Define the syntax error score for each illegal closing character
    error_scores = {
        ')': 3,
        ']': 57,
        '}': 1197,
        '>': 25137
    }
    
    total_syntax_error_score = 0

    for line in lines:
        stack = []
        for char in line:
            if char in "([{<":  # If it's an opening character, push to stack
                stack.append(char)
            elif char in ")]}>":  # If it's a closing character
                if stack and stack[-1] == matching_pairs[char]:
                    stack.pop()  # Correct closing character, pop the matching opening character
                else:
                    # Found an illegal closing character
                    total_syntax_error_score += error_scores[char]
                    break  # Stop processing this line

    return total_syntax_error_score

if __name__ == "__main__":
    # Read input from the file
    lines = read_input('input_level_10.txt')
    
    # Calculate the total syntax error score
    total_score = calculate_syntax_error_score(lines)
    
    print(f"The total syntax error score is: {total_score}")
