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

  def potential_energy
    @position[:x].abs + @position[:y].abs + @position[:z].abs
  end

  def kinetic_energy
    @velocity[:x].abs + @velocity[:y].abs + @velocity[:z].abs
  end

  def total_energy
    potential_energy * kinetic_energy
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

def simulate(moons, steps)
  steps.times do
    moons.combination(2).each do |moon1, moon2|
      moon1.apply_gravity(moon2)
    end
    moons.each(&:apply_velocity)
  end
end

def total_system_energy(moons)
  moons.sum(&:total_energy)
end

def main
  moons = parse_input('input_level_12.txt')
  simulate(moons, 1000)
  puts "Total energy after 1000 steps: #{total_system_energy(moons)}"
end

main
