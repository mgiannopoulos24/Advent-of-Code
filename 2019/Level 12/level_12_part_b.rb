require 'set'

class Moon
  attr_accessor :position, :velocity

  def initialize(position)
    @position = position
    @velocity = { x: 0, y: 0, z: 0 }
  end

  def apply_gravity(other)
    [:x, :y, :z].each do |axis|
      if @position[axis] < other.position[axis]
        @velocity[axis] += 1
        other.velocity[axis] -= 1
      elsif @position[axis] > other.position[axis]
        @velocity[axis] -= 1
        other.velocity[axis] += 1
      end
    end
  end

  def apply_velocity
    @position[:x] += @velocity[:x]
    @position[:y] += @velocity[:y]
    @position[:z] += @velocity[:z]
  end
end

def parse_input(file)
  moons = []
  File.readlines(file).each do |line|
    position = line.scan(/-?\d+/).map(&:to_i)
    moons << Moon.new({ x: position[0], y: position[1], z: position[2] })
  end
  moons
end

def simulate_until_repeat(moons)
  initial_state = {}
  cycle_lengths = {}

  # Track initial states for each axis (x, y, z)
  [:x, :y, :z].each do |axis|
    initial_state[axis] = moons.map { |moon| [moon.position[axis], moon.velocity[axis]] }
    cycle_lengths[axis] = nil
  end

  steps = 0

  until cycle_lengths.values.all?
    # Apply gravity to all pairs of moons
    moons.combination(2).each do |moon1, moon2|
      moon1.apply_gravity(moon2)
    end

    # Apply velocity to all moons
    moons.each(&:apply_velocity)

    steps += 1

    # Check for cycle in each axis
    [:x, :y, :z].each do |axis|
      if cycle_lengths[axis].nil? &&
         moons.map { |moon| [moon.position[axis], moon.velocity[axis]] } == initial_state[axis]
        cycle_lengths[axis] = steps
      end
    end
  end

  # Find LCM of cycle lengths
  cycle_lengths.values.reduce(1) { |lcm, cycle| lcm.lcm(cycle) }
end

# Main execution
if __FILE__ == $PROGRAM_NAME
  moons = parse_input('input_level_12.txt')
  result = simulate_until_repeat(moons)
  puts "Steps until the system repeats: #{result}"
end
