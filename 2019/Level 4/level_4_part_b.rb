def valid_password?(password)
    digits = password.to_s.chars.map(&:to_i)
    has_exact_pair = false
  
    # Check for non-decreasing order
    digits.each_cons(2) do |a, b|
      return false if a > b
    end
  
    # Check for exactly two adjacent matching digits (not part of a larger group)
    counts = Hash.new(0)
    digits.each { |digit| counts[digit] += 1 }
  
    counts.each_value do |count|
      if count == 2
        has_exact_pair = true
        break
      end
    end
  
    has_exact_pair
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
  