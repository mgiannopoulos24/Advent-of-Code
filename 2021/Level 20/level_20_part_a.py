def parse_input(file_name):
    with open(file_name, 'r') as f:
        lines = f.read().splitlines()
    
    enhancement_algorithm = lines[0]
    image = [list(line) for line in lines[2:]]  # Skips the empty line and algorithm part
    return enhancement_algorithm, image

# Function to pad the image with dark pixels to simulate an infinite grid
def pad_image(image, pad_char, pad_size=2):
    rows = len(image)
    cols = len(image[0])
    
    # Pad top and bottom
    padded_image = [[pad_char] * (cols + 2 * pad_size) for _ in range(pad_size)]
    for row in image:
        padded_image.append([pad_char] * pad_size + row + [pad_char] * pad_size)
    padded_image += [[pad_char] * (cols + 2 * pad_size) for _ in range(pad_size)]
    
    return padded_image

# Function to get the binary number formed by the 3x3 neighborhood around (r, c)
def get_pixel_value(image, r, c, default_char):
    binary_string = ''
    for dr in range(-1, 2):
        for dc in range(-1, 2):
            nr, nc = r + dr, c + dc
            if 0 <= nr < len(image) and 0 <= nc < len(image[0]):
                binary_string += '1' if image[nr][nc] == '#' else '0'
            else:
                binary_string += '1' if default_char == '#' else '0'
    return int(binary_string, 2)

# Function to enhance the image based on the algorithm
def enhance_image(image, enhancement_algorithm, default_char):
    new_image = []
    for r in range(len(image)):
        new_row = []
        for c in range(len(image[0])):
            pixel_value = get_pixel_value(image, r, c, default_char)
            new_row.append(enhancement_algorithm[pixel_value])
        new_image.append(new_row)
    return new_image

# Main function to process the image
def process_image(file_name, steps):
    enhancement_algorithm, image = parse_input(file_name)
    
    default_char = '.'
    
    for step in range(steps):
        # Pad the image with the default char (dark pixels)
        image = pad_image(image, default_char, pad_size=2)
        
        # Enhance the image
        image = enhance_image(image, enhancement_algorithm, default_char)
        
        # Toggle the default char if the first char in the algorithm is '#'
        if enhancement_algorithm[0] == '#':
            default_char = '#' if default_char == '.' else '.'
    
    # Count lit pixels
    lit_pixels = sum(row.count('#') for row in image)
    
    return lit_pixels

# Example usage
if __name__ == "__main__":
    result = process_image("input_level_20.txt", 2)
    print(f"Number of lit pixels after 2 enhancements: {result}")
