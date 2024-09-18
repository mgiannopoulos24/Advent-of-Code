def read_puzzle(filename):
    # Read the puzzle input from a file
    with open(filename) as f:
        return [row.split() for row in f.read().split("\n")]

def get_relevant_adds(puzzle):
    # Extract relevant adds for div1 and div26 operations
    div1, div26 = [], []
    for i in range(0, len(puzzle), 18):
        if puzzle[i + 4][2] == "1":
            div1.append(int(puzzle[i + 15][2]))
            div26.append(None)
        else:
            div1.append(None)
            div26.append(int(puzzle[i + 5][2]))
    return div1, div26

def get_largest_model_no(div1, div26):
    # Calculate the largest valid model number based on the div1 and div26 instructions
    modelNo = [0] * 14
    stack = []
    startDigit = 9  # For part 1, we maximize the digits

    for i, (a, b) in enumerate(zip(div1, div26)):
        if a is not None:
            # If it's a "push" operation (div1), we add to the stack
            stack.append((i, a))
        else:
            # If it's a "pop" operation (div26), we pop from the stack
            ia, a = stack.pop()
            diff = a + b
            modelNo[ia] = min(startDigit, startDigit - diff)
            modelNo[i] = min(startDigit, startDigit + diff)

    return modelNo

def solve_part1(puzzle):
    # Solve part 1 by getting the relevant adds and computing the largest model number
    div1, div26 = get_relevant_adds(puzzle)
    return "".join(map(str, get_largest_model_no(div1, div26)))

if __name__ == "__main__":

    # Read puzzle input from a file
    puzzle = read_puzzle("input_level_24.txt")

    # Solve Part 1 (find the largest valid model number)
    print("Part 1 (Largest model number):", solve_part1(puzzle))
