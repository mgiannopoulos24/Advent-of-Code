require 'set'

def read_grid(filename)
    File.readlines(filename).map { |line| line.strip.chars }
  end
  
  def print_grid(grid)
    grid.each { |row| puts row.join }
    puts
  end
  
  def get_adjacent_bugs(grid, x, y)
    adjacent_positions = [[-1, 0], [1, 0], [0, -1], [0, 1]]
    count = 0
    adjacent_positions.each do |dx, dy|
      nx, ny = x + dx, y + dy
      if (0 <= nx && nx < 5) && (0 <= ny && ny < 5)
        count += 1 if grid[nx][ny] == '#'
      end
    end
    count
  end
  
  def next_state(grid)
    new_grid = Array.new(5) { Array.new(5, '.') }
    (0...5).each do |x|
      (0...5).each do |y|
        bugs_around = get_adjacent_bugs(grid, x, y)
        if grid[x][y] == '#' && bugs_around != 1
          new_grid[x][y] = '.'
        elsif grid[x][y] == '.' && (bugs_around == 1 || bugs_around == 2)
          new_grid[x][y] = '#'
        else
          new_grid[x][y] = grid[x][y]
        end
      end
    end
    new_grid
  end
  
  def grid_to_tuple(grid)
    grid.map(&:dup).map { |row| row.dup }.freeze
  end
  
  def calculate_biodiversity(grid)
    biodiversity = 0
    power = 1
    grid.each do |row|
      row.each do |cell|
        if cell == '#'
          biodiversity += power
        end
        power *= 2
      end
    end
    biodiversity
  end
  
  def main(filename)
    grid = read_grid(filename)
    seen_layouts = Set.new
    loop do
      layout_tuple = grid_to_tuple(grid)
      if seen_layouts.include?(layout_tuple)
        return calculate_biodiversity(grid)
      end
      seen_layouts.add(layout_tuple)
      grid = next_state(grid)
    end
  end
  
  filename = 'input_level_24.txt' 
  puts main(filename)
  