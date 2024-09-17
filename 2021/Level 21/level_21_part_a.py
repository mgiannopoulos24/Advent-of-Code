def parse_input(file_name):
    with open(file_name, 'r') as file:
        lines = file.read().strip().splitlines()
        player1_start = int(lines[0].split(": ")[1])
        player2_start = int(lines[1].split(": ")[1])
    return player1_start, player2_start

# Function to simulate the game with a deterministic die
def play_dirac_dice(player1_start, player2_start):
    # Initial positions and scores
    positions = [player1_start, player2_start]
    scores = [0, 0]
    
    # Deterministic die and roll counter
    die = 1
    rolls = 0
    current_player = 0
    
    def roll_dice():
        nonlocal die, rolls
        total = 0
        for _ in range(3):
            total += die
            die += 1
            rolls += 1
            if die > 100:
                die = 1
        return total

    # Game loop
    while True:
        # Current player rolls the dice
        move = roll_dice()
        positions[current_player] = (positions[current_player] + move - 1) % 10 + 1
        scores[current_player] += positions[current_player]
        
        # Check for a winner
        if scores[current_player] >= 1000:
            break
        
        # Switch to the other player
        current_player = 1 - current_player

    # Calculate the result: losing player's score * number of rolls
    losing_player_score = scores[1 - current_player]
    return losing_player_score * rolls

# Main function to read input and start the game
def main(file_name):
    player1_start, player2_start = parse_input(file_name)
    result = play_dirac_dice(player1_start, player2_start)
    print(f"Result: {result}")

# Example usage
if __name__ == "__main__":
    main("input_level_21.txt")
