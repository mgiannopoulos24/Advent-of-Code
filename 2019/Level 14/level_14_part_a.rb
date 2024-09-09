# Parsing reactions and calculating required ORE for 1 FUEL

# Function to parse the reactions from the input
def parse_reactions(input)
    reactions = {}
    input.each_line do |line|
      inputs, output = line.strip.split(' => ')
      output_qty, output_chem = output.split
      inputs = inputs.split(', ').map do |input|
        qty, chem = input.split
        [chem, qty.to_i]
      end
      reactions[output_chem] = { quantity: output_qty.to_i, inputs: inputs }
    end
    reactions
  end
  
  # Recursive function to calculate the ORE needed for a certain amount of a chemical
  def ore_required_for(chemical, amount, reactions, leftovers)
    return amount if chemical == 'ORE'
  
    if leftovers[chemical] >= amount
      leftovers[chemical] -= amount
      return 0
    elsif leftovers[chemical] > 0
      amount -= leftovers[chemical]
      leftovers[chemical] = 0
    end
  
    reaction = reactions[chemical]
    batch_size = reaction[:quantity]
    times = (amount.to_f / batch_size).ceil
  
    total_ore = 0
    reaction[:inputs].each do |input_chem, input_qty|
      total_ore += ore_required_for(input_chem, input_qty * times, reactions, leftovers)
    end
  
    leftovers[chemical] += (times * batch_size) - amount
    total_ore
  end
  
  # Main function to calculate the minimum ORE needed for 1 FUEL
  def minimum_ore_for_fuel(reactions)
    leftovers = Hash.new(0)
    ore_required_for('FUEL', 1, reactions, leftovers)
  end
  
  # Main execution
  if __FILE__ == $PROGRAM_NAME
    filename = 'input_level_14.txt'
    input = File.read(filename)
    reactions = parse_reactions(input)
    ore_needed = minimum_ore_for_fuel(reactions)
    puts "Minimum ORE required for 1 FUEL: #{ore_needed}"
  end
  