def read_input(filename):
    with open(filename, 'r') as f:
        # Read the first line for the numbers to draw
        numbers = list(map(int, f.readline().strip().split(',')))
        
        # Read the boards
        boards = []
        current_board = []
        for line in f:
            line = line.strip()
            if line == "":
                if current_board:
                    boards.append(current_board)
                    current_board = []
            else:
                current_board.append(list(map(int, line.split())))
        if current_board:
            boards.append(current_board)
    
    return numbers, boards

def mark_board(board, number):
    for row in board:
        for i in range(len(row)):
            if row[i] == number:
                row[i] = None  # Mark the number as None

def check_winner(board):
    # Check rows
    for row in board:
        if all(x is None for x in row):
            return True
    
    # Check columns
    for col in range(5):
        if all(row[col] is None for row in board):
            return True
    
    return False

def calculate_score(board, last_number):
    unmarked_sum = sum(sum(x for x in row if x is not None) for row in board)
    return unmarked_sum * last_number

def play_bingo(numbers, boards):
    for number in numbers:
        for board in boards:
            mark_board(board, number)
            if check_winner(board):
                return calculate_score(board, number)

if __name__ == "__main__":
    numbers, boards = read_input('input_level_4.txt') # Replace with the actual input file name
    score = play_bingo(numbers, boards)
    print(f"The final score of the first winning board is: {score}")
