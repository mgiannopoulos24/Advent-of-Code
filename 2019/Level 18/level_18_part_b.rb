class Vector2d
    attr_accessor :x, :y
  
    def initialize(x, y)
      @x = x
      @y = y
    end
  
    def ==(other)
      @x == other.x && @y == other.y
    end
  
    def eql?(other)
      self == other
    end
  
    def hash
      [@x, @y].hash
    end
  end
  
  class Vector3d
    attr_accessor :x, :y, :z
  
    def initialize(x, y, z)
      @x = x
      @y = y
      @z = z
    end
  
    def ==(other)
      @x == other.x && @y == other.y && @z == other.z
    end
  
    def eql?(other)
      self == other
    end
  
    def hash
      [@x, @y, @z].hash
    end
  end
  
  class State
    attr_accessor :pos, :steps
  
    def initialize(pos, steps)
      @pos = pos
      @steps = steps
    end
  end
  
  def get_grid
    grid, doors, keys = {}, {}, {}
    input = File.read('input_level_18.txt')
    x, y = 0, 0
    start = nil
    all_keys = 0
  
    input.each_line do |line|
      line.each_char do |c|
        unless c == '#'
          grid[Vector2d.new(x, y)] = true
          if c == '@'
            start = Vector2d.new(x, y)
          elsif c != '.'
            k = if c < 'a'
                  1 << (c.ord - 'A'.ord)
                else
                  1 << (c.ord - 'a'.ord)
                end
            if c < 'a'
              doors[Vector2d.new(x, y)] = k
            else
              keys[Vector2d.new(x, y)] = k
            end
            all_keys |= k
          end
        end
        x += 1
      end
      x = 0
      y += 1
    end
    [grid, doors, keys, start, all_keys]
  end
  
  def search(grid, doors, keys, start, all_keys, have_keys)
    directions = [Vector2d.new(0, -1), Vector2d.new(1, 0), Vector2d.new(0, 1), Vector2d.new(-1, 0)]
    queue = [State.new(Vector3d.new(start.x, start.y, have_keys), 0)]
    visited = {}
  
    until queue.empty?
      st = queue.shift
  
      return st.steps if (st.pos.z & all_keys) == all_keys
  
      visited[st.pos] = true
  
      directions.each do |d|
        next_pos = Vector3d.new(st.pos.x + d.x, st.pos.y + d.y, st.pos.z)
  
        next unless grid[Vector2d.new(next_pos.x, next_pos.y)]
        next if visited[next_pos]
  
        if (door = doors[Vector2d.new(next_pos.x, next_pos.y)])
          next unless (next_pos.z & door) == door
        end
  
        if (key = keys[Vector2d.new(next_pos.x, next_pos.y)])
          next_pos.z |= key
        end
  
        queue << State.new(next_pos, st.steps + 1)
      end
    end
  end
  
  def part2
    grid, doors, keys, start, all_keys = get_grid
  
    directions = [Vector2d.new(0, -1), Vector2d.new(1, 0), Vector2d.new(0, 1), Vector2d.new(-1, 0)]
    grid[start] = false
    directions.each do |d|
      grid[Vector2d.new(start.x + d.x, start.y + d.y)] = false
    end
  
    total = 0
  
    # Quadrant 1 (Top-left)
    have_keys = all_keys
    (0...start.x).each do |x|
      (0...start.y).each do |y|
        have_keys ^= keys[Vector2d.new(x, y)] if keys[Vector2d.new(x, y)]
      end
    end
    total += search(grid, doors, keys, Vector2d.new(start.x - 1, start.y - 1), all_keys, have_keys)
  
    # Quadrant 2 (Top-right)
    have_keys = all_keys
    ((start.x + 1)..(start.x * 2)).each do |x|
      (0...start.y).each do |y|
        have_keys ^= keys[Vector2d.new(x, y)] if keys[Vector2d.new(x, y)]
      end
    end
    total += search(grid, doors, keys, Vector2d.new(start.x + 1, start.y - 1), all_keys, have_keys)
  
    # Quadrant 3 (Bottom-right)
    have_keys = all_keys
    ((start.x + 1)..(start.x * 2)).each do |x|
      ((start.y + 1)..(start.y * 2)).each do |y|
        have_keys ^= keys[Vector2d.new(x, y)] if keys[Vector2d.new(x, y)]
      end
    end
    total += search(grid, doors, keys, Vector2d.new(start.x + 1, start.y + 1), all_keys, have_keys)
  
    # Quadrant 4 (Bottom-left)
    have_keys = all_keys
    (0...start.x).each do |x|
      ((start.y + 1)..(start.y * 2)).each do |y|
        have_keys ^= keys[Vector2d.new(x, y)] if keys[Vector2d.new(x, y)]
      end
    end
    total += search(grid, doors, keys, Vector2d.new(start.x - 1, start.y + 1), all_keys, have_keys)
  
    puts "The shortest path to collect all keys:", total
  end
  
  part2
  