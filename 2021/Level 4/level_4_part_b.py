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

def play_bingo_until_last(numbers, boards):
    won_boards = set()  # To track boards that have already won
    last_score = None
    
    for number in numbers:
        for idx, board in enumerate(boards):
            if idx in won_boards:
                continue  # Skip this board if it has already won
            
            mark_board(board, number)
            if check_winner(board):
                won_boards.add(idx)
                last_score = calculate_score(board, number)
            
            # If all boards have won, return the score of the last one
            if len(won_boards) == len(boards):
                return last_score

if __name__ == "__main__":
    numbers, boards = read_input('input_level_4.txt') # Replace with the actual input file name
    score = play_bingo_until_last(numbers, boards)
    print(f"The final score of the last winning board is: {score}")
