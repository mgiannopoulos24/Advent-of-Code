def read_input(filename):
    with open(filename, 'r') as file:
        # Read the input and split by commas into a list of integers
        initial_timers = list(map(int, file.readline().strip().split(',')))
    return initial_timers

def simulate_lanternfish(timers, days):
    # Initialize a list to count fish by their timer values (from 0 to 8)
    fish_counts = [0] * 9
    
    # Populate the initial fish counts from the input timers
    for timer in timers:
        fish_counts[timer] += 1
    
    # Simulate each day
    for day in range(days):
        # The number of fish with timer 0 (that will reset and spawn new fish)
        new_fish = fish_counts[0]
        
        # Shift all fish timers down by 1
        for i in range(8):
            fish_counts[i] = fish_counts[i + 1]
        
        # Fish that were at timer 0 reset to timer 6
        fish_counts[6] += new_fish
        
        # New fish are added at timer 8
        fish_counts[8] = new_fish
    
    # Return the total number of fish after all days
    return sum(fish_counts)

if __name__ == "__main__":
    initial_timers = read_input('input_level_6.txt') # Replace with the actual input file name
    
    # Simulate for 256 days
    total_fish = simulate_lanternfish(initial_timers, 256)
    
    print(f"Total number of lanternfish after 256 days: {total_fish}")
