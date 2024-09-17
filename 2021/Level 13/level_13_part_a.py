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

if __name__ == "__main__":
    # Read input
    dots, folds = read_input('input_level_13.txt')
    
    # Perform the first fold
    first_fold_instruction = folds[0]
    dots_after_first_fold = fold_paper(dots, first_fold_instruction)
    
    # Count the number of unique dots after the first fold
    print(f"Number of visible dots after the first fold: {len(dots_after_first_fold)}")
