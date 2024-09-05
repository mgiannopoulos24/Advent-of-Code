# Define the dimensions of the image
WIDTH = 25
HEIGHT = 6
LAYER_SIZE = WIDTH * HEIGHT

# Read the input from the text file
input = File.read("input_level_8.txt").chomp

# Split the input into layers of size WIDTH * HEIGHT
layers = input.chars.each_slice(LAYER_SIZE).map(&:join)

# Find the layer with the fewest '0' digits
fewest_zero_layer = layers.min_by { |layer| layer.count('0') }

# Count the number of '1' digits and '2' digits in that layer
ones_count = fewest_zero_layer.count('1')
twos_count = fewest_zero_layer.count('2')

# Multiply the count of '1's by the count of '2's
result = ones_count * twos_count

# Output the result
puts "The result is: #{result}"
