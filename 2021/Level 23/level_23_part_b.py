import heapq

# Define the energy cost per step for each amphipod type
energy_cost = {
    'A': 1,    # Amber
    'B': 10,   # Bronze
    'C': 100,  # Copper
    'D': 1000  # Desert
}

# Target positions for each amphipod type in the rooms (2, 4, 6, 8 correspond to the rooms in the hallway)
room_target = {
    'A': 2,
    'B': 4,
    'C': 6,
    'D': 8
}

# Define the burrow's layout
hallway_length = 11
room_depth = 4  # Four rows deep

# Parse the input to get the initial state
def parse_input(input_lines):
    hallway = ['.'] * hallway_length
    rooms = [
        [input_lines[2][3], input_lines[3][3], input_lines[4][3], input_lines[5][3]],  # Room for A
        [input_lines[2][5], input_lines[3][5], input_lines[4][5], input_lines[5][5]],  # Room for B
        [input_lines[2][7], input_lines[3][7], input_lines[4][7], input_lines[5][7]],  # Room for C
        [input_lines[2][9], input_lines[3][9], input_lines[4][9], input_lines[5][9]]   # Room for D
    ]
    return tuple(hallway), tuple(tuple(r) for r in rooms)

# Check if a path is clear between the hallway and the room
def is_path_clear(hallway, start, end):
    if start < end:
        return all(hallway[i] == '.' for i in range(start + 1, end + 1))
    else:
        return all(hallway[i] == '.' for i in range(end, start))

# Generate all possible moves from the current state
def generate_moves(hallway, rooms):
    moves = []

    # Moves from rooms to hallway
    for room_idx, room in enumerate(rooms):
        room_pos = room_target[chr(room_idx + ord('A'))]
        if any(amphipod != '.' for amphipod in room):
            # Find the amphipod at the top of the stack in this room
            for depth, amphipod in enumerate(room):
                if amphipod != '.':
                    for hall_pos in [0, 1, 3, 5, 7, 9, 10]:
                        if is_path_clear(hallway, room_pos, hall_pos):
                            steps = abs(room_pos - hall_pos) + depth + 1
                            energy = steps * energy_cost[amphipod]
                            new_hallway = list(hallway)
                            new_rooms = list(list(r) for r in rooms)
                            new_hallway[hall_pos] = amphipod
                            new_rooms[room_idx][depth] = '.'
                            moves.append((tuple(new_hallway), tuple(tuple(r) for r in new_rooms), energy))
                    break

    # Moves from hallway to rooms
    for hall_pos, amphipod in enumerate(hallway):
        if amphipod == '.':
            continue
        room_idx = ord(amphipod) - ord('A')
        room_pos = room_target[amphipod]
        room = rooms[room_idx]
        if all(a == '.' or a == amphipod for a in room) and is_path_clear(hallway, hall_pos, room_pos):
            # Amphipod can move into its room
            depth = next(i for i in reversed(range(room_depth)) if room[i] == '.')
            steps = abs(hall_pos - room_pos) + depth + 1
            energy = steps * energy_cost[amphipod]
            new_hallway = list(hallway)
            new_rooms = list(list(r) for r in rooms)
            new_hallway[hall_pos] = '.'
            new_rooms[room_idx][depth] = amphipod
            moves.append((tuple(new_hallway), tuple(tuple(r) for r in new_rooms), energy))

    return moves

# Check if the current state is the goal
def is_goal(rooms):
    return all(
        all(amphipod == chr(i + ord('A')) for amphipod in room)
        for i, room in enumerate(rooms)
    )

# Solve the puzzle using Dijkstra's algorithm
def solve(hallway, rooms):
    initial_state = (hallway, rooms)
    pq = [(0, initial_state)]  # Priority queue with (energy, state)
    visited = set()
    
    while pq:
        energy, (current_hallway, current_rooms) = heapq.heappop(pq)

        if is_goal(current_rooms):
            return energy

        if (current_hallway, current_rooms) in visited:
            continue

        visited.add((current_hallway, current_rooms))

        for new_hallway, new_rooms, move_energy in generate_moves(current_hallway, current_rooms):
            heapq.heappush(pq, (energy + move_energy, (new_hallway, new_rooms)))

    return -1  # Shouldn't happen if the puzzle is solvable

# Input puzzle configuration for Part 2
puzzle_input = [
    "#############",
    "#...........#",
    "###D#B#D#A###",
    "  #D#C#B#A#",
    "  #D#B#A#C#",
    "  #C#C#A#B#",
    "  #########"
]

# Parse the puzzle input
hallway, rooms = parse_input(puzzle_input)

# Solve the puzzle
min_energy = solve(hallway, rooms)
print("Least energy required:",min_energy)
