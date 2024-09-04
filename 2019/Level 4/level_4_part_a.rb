def valid_password?(password)
    digits = password.to_s.chars.map(&:to_i)
    
    has_adjacent = false
    digits.each_cons(2) do |a, b|
      if a == b
        has_adjacent = true
      end
      return false if a > b
    end
    
    has_adjacent
  end
  
  def count_valid_passwords(range_start, range_end)
    count = 0
    (range_start..range_end).each do |password|
      count += 1 if valid_password?(password)
    end
    count
  end
  
  # Puzzle input range
  range_start = 278384
  range_end = 824795
  
  # Calculate and print the number of valid passwords
  puts count_valid_passwords(range_start, range_end)
  