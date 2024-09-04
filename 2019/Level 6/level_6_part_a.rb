def load_orbit_map(filename)
    File.readlines(filename).map(&:strip)
  end
  
  def build_orbit_tree(orbits)
    tree = {}
    orbits.each do |orbit|
      parent, child = orbit.split(')')
      tree[child] = parent
    end
    tree
  end
  
  def count_orbits(tree, object)
    count = 0
    while tree[object]
      count += 1
      object = tree[object]
    end
    count
  end
  
  def total_orbits(tree)
    total = 0
    tree.keys.each do |object|
      total += count_orbits(tree, object)
    end
    total
  end
  
  # Load the orbit map from the file
  orbit_map = load_orbit_map('input_level_6.txt')
  
  # Build the orbit tree from the map
  orbit_tree = build_orbit_tree(orbit_map)
  
  # Calculate the total number of direct and indirect orbits
  total_orbit_count = total_orbits(orbit_tree)
  
  # Print the result
  puts "Total number of direct and indirect orbits: #{total_orbit_count}"
  