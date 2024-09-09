# File path to read instructions
file_path = 'input_level_22.txt'

# Initialize the deck
deck_size = 10007
deck = (0...deck_size).to_a

# Read the shuffle instructions from the file
instructions = File.readlines(file_path).map(&:strip)

# Define the shuffle techniques
instructions.each do |instruction|
  if instruction == 'deal into new stack'
    deck.reverse!
  elsif instruction.start_with?('cut')
    n = instruction.split.last.to_i
    deck = deck.rotate(n)
  elsif instruction.start_with?('deal with increment')
    increment = instruction.split.last.to_i
    new_deck = Array.new(deck_size)
    deck.each_with_index do |card, index|
      new_position = (index * increment) % deck_size
      new_deck[new_position] = card
    end
    deck = new_deck
  end
end

# Find the position of card 2019
position_of_2019 = deck.index(2019)
puts "The position of card 2019 is: #{position_of_2019}"
