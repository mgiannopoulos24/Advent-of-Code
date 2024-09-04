def calculate_fuel(mass)
    # Calculate the fuel needed for a given mass
    (mass / 3).floor - 2
  end
  
  def calculate_total_fuel(mass)
    total_fuel = 0
    fuel = calculate_fuel(mass)
    
    while fuel > 0
      total_fuel += fuel
      fuel = calculate_fuel(fuel)
    end
    
    total_fuel
  end
  
  def total_fuel_requirements(file_path)
    total_fuel = 0
    
    # Read the file line by line
    File.foreach(file_path) do |line|
      # Convert each line to an integer
      mass = line.to_i
      # Calculate total fuel including fuel for the fuel
      total_fuel += calculate_total_fuel(mass)
    end
    
    total_fuel
  end

    # Path to the input file
    input_file = 'input_level_1.txt'
    # Calculate and print the total fuel requirement
    puts total_fuel_requirements(input_file)
    