# File path to read instructions
file_path = 'input_level_22.txt'

# Constants for part two
deck_size = 119315717514047
shuffles = 101741582076661
target_position = 2020

# Read the shuffle instructions from the file
instructions = File.readlines(file_path).map(&:strip)

# Modulo inverse function
def modinv(a, m)
  a.pow(m - 2, m)
end

# Initialize transformation parameters a and b for the linear transformation
a = 1
b = 0

# Process each shuffle instruction in reverse
instructions.reverse.each do |instruction|
  if instruction == 'deal into new stack'
    a = -a % deck_size
    b = (-b - 1) % deck_size
  elsif instruction.start_with?('cut')
    n = instruction.split.last.to_i
    b = (b + n) % deck_size
  elsif instruction.start_with?('deal with increment')
    increment = instruction.split.last.to_i
    inv = modinv(increment, deck_size)
    a = (a * inv) % deck_size
    b = (b * inv) % deck_size
  end
end

# Function to apply repeated shuffling using modular arithmetic
def apply_shuffle(a, b, shuffles, deck_size)
  a_n = a.pow(shuffles, deck_size)
  b_n = (b * (1 - a_n) * modinv(1 - a, deck_size)) % deck_size
  return a_n, b_n
end

# Apply the shuffle transformations
a_n, b_n = apply_shuffle(a, b, shuffles, deck_size)

# Calculate the number on the card in the target position
card_at_position = (a_n * target_position + b_n) % deck_size

puts "The card at position #{target_position} is: #{card_at_position}"
