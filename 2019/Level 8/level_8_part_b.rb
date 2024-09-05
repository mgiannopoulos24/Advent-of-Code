# Define the dimensions of the image
WIDTH = 25
HEIGHT = 6
LAYER_SIZE = WIDTH * HEIGHT

# Read the input from the text file
input = File.read("input_level_8.txt").chomp

# Split the input into layers of size WIDTH * HEIGHT
layers = input.chars.each_slice(LAYER_SIZE).to_a

# Initialize the final image as transparent pixels (2 represents transparent)
final_image = Array.new(LAYER_SIZE, '2')

# Iterate through each pixel position in the image
(0...LAYER_SIZE).each do |i|
  # For each position, find the first non-transparent pixel (not '2')
  layers.each do |layer|
    if layer[i] != '2'
      final_image[i] = layer[i]
      break
    end
  end
end

# Now, print the final image in a readable way
# '0' is black, '1' is white, so we'll print ' ' for black and '#' for white
final_image.each_slice(WIDTH) do |row|
  puts row.map { |pixel| pixel == '1' ? '#' : ' ' }.join
end
