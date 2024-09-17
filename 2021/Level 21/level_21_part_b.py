from collections import defaultdict

def parse_input(file_name):
    with open(file_name, 'r') as file:
        lines = file.read().strip().splitlines()
        player1_start = int(lines[0].split(": ")[1])
        player2_start = int(lines[1].split(": ")[1])
    return player1_start, player2_start

# Possible sums of three rolls of a quantum 3-sided die, and how often each sum appears
quantum_die_outcomes = defaultdict(int)
for roll1 in [1, 2, 3]:
    for roll2 in [1, 2, 3]:
        for roll3 in [1, 2, 3]:
            quantum_die_outcomes[roll1 + roll2 + roll3] += 1

# Memoization dictionary to store the results of each game state
memo = {}

# Recursive function to simulate the game with Dirac dice
def dirac_dice(p1_pos, p2_pos, p1_score, p2_score, turn):
    # If this game state has been computed before, return the memoized result
    if (p1_pos, p2_pos, p1_score, p2_score, turn) in memo:
        return memo[(p1_pos, p2_pos, p1_score, p2_score, turn)]
    
    # If either player has reached or exceeded 21 points, the game ends
    if p1_score >= 21:
        return (1, 0)  # Player 1 wins in this universe
    if p2_score >= 21:
        return (0, 1)  # Player 2 wins in this universe

    p1_wins, p2_wins = 0, 0

    # It's Player 1's turn
    if turn == 1:
        for roll_sum, count in quantum_die_outcomes.items():
            new_p1_pos = (p1_pos + roll_sum - 1) % 10 + 1  # Move Player 1
            new_p1_score = p1_score + new_p1_pos  # Update Player 1's score
            wins1, wins2 = dirac_dice(new_p1_pos, p2_pos, new_p1_score, p2_score, 2)
            p1_wins += wins1 * count
            p2_wins += wins2 * count
    # It's Player 2's turn
    else:
        for roll_sum, count in quantum_die_outcomes.items():
            new_p2_pos = (p2_pos + roll_sum - 1) % 10 + 1  # Move Player 2
            new_p2_score = p2_score + new_p2_pos  # Update Player 2's score
            wins1, wins2 = dirac_dice(p1_pos, new_p2_pos, p1_score, new_p2_score, 1)
            p1_wins += wins1 * count
            p2_wins += wins2 * count

    # Memoize the result for this game state
    memo[(p1_pos, p2_pos, p1_score, p2_score, turn)] = (p1_wins, p2_wins)
    return p1_wins, p2_wins

# Main function to start the quantum game
def main(file_name):
    # Parse the starting positions of both players
    player1_start, player2_start = parse_input(file_name)

    # Reset the memoization dictionary before starting the simulation
    memo.clear()

    # Start the simulation, with Player 1 starting the first turn
    p1_wins, p2_wins = dirac_dice(player1_start, player2_start, 0, 0, 1)

    # Output the number of universes where each player wins
    print(f"Player 1 wins in {p1_wins} universes.")
    print(f"Player 2 wins in {p2_wins} universes.")
    print(f"The player who wins in more universes wins in {max(p1_wins, p2_wins)} universes.")

# Example usage
if __name__ == "__main__":
    main("input_level_21.txt")
