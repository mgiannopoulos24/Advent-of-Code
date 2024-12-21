from functools import cache
from itertools import permutations

# Define the keypad layout and directions
keypad_layout = ['789',
                 '456',
                 '123',
                 ' 0A']
keypad = {char: (x, y) for y, row in enumerate(keypad_layout) for x, char in enumerate(row) if char != ' '}

direction_layout = [' ^A',
                    '<v>']
directions = {char: (x, y) for y, row in enumerate(direction_layout) for x, char in enumerate(row) if char != ' '}

move_map = {'^': (0, -1), '>': (1, 0), 'v': (0, 1), '<': (-1, 0)}

# Function for calculating minimum button presses
@cache
def calculate_button_presses(sequence, max_depth=2, use_direction=False, current_pos=None):
    active_keypad = directions if use_direction else keypad  # Use the correct dictionary (keypad or directions)
    if not sequence:
        return 0
    if not current_pos:
        current_pos = active_keypad['A']  # Ensure 'A' is accessed from the dictionary, not the list

    current_x, current_y = current_pos
    target_x, target_y = active_keypad[sequence[0]]  # Correct dictionary reference here
    delta_x, delta_y = target_x - current_x, target_y - current_y

    move_sequence = ''
    if delta_x > 0:
        move_sequence += '>' * delta_x
    elif delta_x < 0:
        move_sequence += '<' * -delta_x
    if delta_y > 0:
        move_sequence += 'v' * delta_y
    elif delta_y < 0:
        move_sequence += '^' * -delta_y

    if max_depth:
        perm_lengths = []
        for perm in set(permutations(move_sequence)):
            temp_x, temp_y = current_pos
            for move in perm:
                delta_x, delta_y = move_map[move]
                temp_x += delta_x
                temp_y += delta_y
                if (temp_x, temp_y) not in active_keypad.values():
                    break
            else:
                perm_lengths.append(calculate_button_presses(perm + ('A',), max_depth - 1, True))
        min_perm_length = min(perm_lengths)
    else:
        min_perm_length = len(move_sequence) + 1

    return min_perm_length + calculate_button_presses(sequence[1:], max_depth, use_direction, (target_x, target_y))

with open('input_level_21.txt') as file:
    input_codes = file.read().splitlines()

total_presses = 0
for code in input_codes:
    code_value = int(code[:-1])
    total_presses += code_value * calculate_button_presses(code)

print(total_presses)
