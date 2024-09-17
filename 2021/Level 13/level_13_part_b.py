def read_input(filename):
    with open(filename, 'r') as file:
        lines = [line.strip() for line in file.readlines()]
    
    dots = set()
    folds = []
    
    # Parse dots and fold instructions
    for line in lines:
        if ',' in line:
            x, y = map(int, line.split(','))
            dots.add((x, y))
        elif 'fold along' in line:
            axis, value = line.split()[-1].split('=')
            folds.append((axis, int(value)))
    
    return dots, folds

def fold_paper(dots, fold_instruction):
    axis, fold_line = fold_instruction
    new_dots = set()

    for x, y in dots:
        if axis == 'x':
            # Fold along vertical line (fold to the left)
            if x > fold_line:
                x = fold_line - (x - fold_line)
        elif axis == 'y':
            # Fold along horizontal line (fold upward)
            if y > fold_line:
                y = fold_line - (y - fold_line)
        
        new_dots.add((x, y))
    
    return new_dots

def print_grid(dots):
    # Find the size of the grid
    max_x = max(x for x, y in dots)
    max_y = max(y for x, y in dots)
    
    # Create the grid
    grid = [['.' for _ in range(max_x + 1)] for _ in range(max_y + 1)]
    
    # Mark the dots
    for x, y in dots:
        grid[y][x] = '#'
    
    # Print the grid row by row
    for row in grid:
        print(''.join(row))

if __name__ == "__main__":
    # Read input
    dots, folds = read_input('input_level_13.txt')
    
    # Perform all folds
    for fold_instruction in folds:
        dots = fold_paper(dots, fold_instruction)
    
    # Print the final grid to reveal the code
    print("The final grid with the code is:")
    print_grid(dots)
