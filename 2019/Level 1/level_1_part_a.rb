def calculate_fuel(mass)
    # Calculate fuel needed for the given mass
    (mass / 3).floor - 2
  end
  
  def total_fuel_requirements(file_path)
    total_fuel = 0
    
    # Read the file line by line
    File.foreach(file_path) do |line|
      # Convert each line to an integer
      mass = line.to_i
      # Calculate fuel for the given mass
      fuel = calculate_fuel(mass)
      # Add the fuel to the total
      total_fuel += fuel
    end
    
    total_fuel
  end
  
  # Path to the input file
  input_file = 'input_level_1.txt'
  # Calculate and print the total fuel requirement
  puts total_fuel_requirements(input_file)
  