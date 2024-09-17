from collections import defaultdict

def read_input(filename):
    lines = []
    with open(filename, 'r') as file:
        for line in file:
            parts = line.strip().split(' -> ')
            start = tuple(map(int, parts[0].split(',')))
            end = tuple(map(int, parts[1].split(',')))
            lines.append((start, end))
    return lines

def get_points_on_line(x1, y1, x2, y2):
    points = []
    if x1 == x2:  # Vertical line
        for y in range(min(y1, y2), max(y1, y2) + 1):
            points.append((x1, y))
    elif y1 == y2:  # Horizontal line
        for x in range(min(x1, x2), max(x1, x2) + 1):
            points.append((x, y1))
    elif abs(x1 - x2) == abs(y1 - y2):  # Diagonal line at 45 degrees
        # Determine the direction of the diagonal
        x_step = 1 if x2 > x1 else -1
        y_step = 1 if y2 > y1 else -1
        for i in range(abs(x1 - x2) + 1):
            points.append((x1 + i * x_step, y1 + i * y_step))
    return points

def count_overlaps(lines):
    point_counts = defaultdict(int)
    
    for (x1, y1), (x2, y2) in lines:
        points = get_points_on_line(x1, y1, x2, y2)
        for point in points:
            point_counts[point] += 1
    
    # Count how many points have 2 or more lines overlapping
    overlap_count = sum(1 for count in point_counts.values() if count >= 2)
    
    return overlap_count

if __name__ == "__main__":
    # Assuming the input is provided in 'input_level_5.txt'
    lines = read_input('input_level_5.txt') # Replace with the actual input file name
    overlap_count = count_overlaps(lines)
    print(f"Number of points where at least two lines overlap: {overlap_count}")
