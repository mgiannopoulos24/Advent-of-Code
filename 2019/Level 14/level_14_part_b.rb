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
  
  # Function to calculate the amount of ORE needed for a given amount of FUEL
  def ore_for_fuel(fuel_amount, reactions)
    leftovers = Hash.new(0)
    ore_required_for('FUEL', fuel_amount, reactions, leftovers)
  end
  
  # Binary search to find the maximum FUEL producible with 1 trillion ORE
  def maximum_fuel_with_trillion_ore(reactions)
    ore_limit = 1_000_000_000_000
    low = 1
    high = ore_limit
  
    while low < high
      mid = (low + high + 1) / 2
      ore_needed = ore_for_fuel(mid, reactions)
  
      if ore_needed > ore_limit
        high = mid - 1
      else
        low = mid
      end
    end
  
    low
  end
  
  # Main execution
  if __FILE__ == $PROGRAM_NAME
    filename = 'input_level_14.txt'
    input = File.read(filename)
    reactions = parse_reactions(input)
  
    # Part 2: Maximum FUEL producible with 1 trillion ORE
    max_fuel = maximum_fuel_with_trillion_ore(reactions)
    puts "Maximum FUEL producible with 1 trillion ORE: #{max_fuel}"
  end
  