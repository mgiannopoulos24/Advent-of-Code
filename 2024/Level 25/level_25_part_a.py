def parse_schematics(input_file):
    with open(input_file, 'r') as file:
        schematics = file.read().strip().split('\n\n')
    locks = []
    keys = []
    
    for schematic in schematics:
        rows = schematic.split('\n')
        top_row = rows[0]
        bottom_row = rows[-1]
        
        if all(char == '#' for char in top_row) and all(char == '.' for char in bottom_row):
            # It's a lock
            locks.append(rows)
        elif all(char == '.' for char in top_row) and all(char == '#' for char in bottom_row):
            # It's a key
            keys.append(rows)
    
    return locks, keys

def convert_to_heights(schematic, is_lock=True):
    height_list = []
    columns = zip(*schematic)  # Transpose rows to columns
    
    for column in columns:
        if is_lock:
            # Count '#' from top down
            height = sum(1 for char in column if char == '#')
        else:
            # Count '#' from bottom up
            height = sum(1 for char in reversed(column) if char == '#')
        height_list.append(height)
    
    return height_list

def count_fitting_pairs(locks, keys):
    lock_heights = [convert_to_heights(lock, is_lock=True) for lock in locks]
    key_heights = [convert_to_heights(key, is_lock=False) for key in keys]
    total_pairs = 0
    
    for lock in lock_heights:
        for key in key_heights:
            if all(lock[col] + key[col] <= len(locks[0]) for col in range(len(lock))):
                total_pairs += 1
    
    return total_pairs

if __name__ == "__main__":
    input_file = "input_level_25.txt" 
    locks, keys = parse_schematics(input_file)
    result = count_fitting_pairs(locks, keys)
    print(f"Number of unique lock/key pairs that fit: {result}")
