def read_program(filename)
    File.read(filename).strip.split(',').map(&:to_i)
  end
  
  def intcode_program(program, input = [])
    pc = 0
    relative_base = 0
    memory = Hash.new(0)
    program.each_with_index { |val, idx| memory[idx] = val }
    
    output = []
    
    while true
      opcode = memory[pc] % 100
      modes = [(memory[pc] / 100) % 10, (memory[pc] / 1000) % 10, (memory[pc] / 10000) % 10]
      
      def get_value(memory, param, mode, relative_base)
        case mode
        when 0
          memory[param] || 0
        when 1
          param
        when 2
          memory[relative_base + param] || 0
        else
          raise "Unknown mode #{mode}"
        end
      end
      
      def set_value(memory, param, mode, relative_base, value)
        case mode
        when 0
          memory[param] = value
        when 2
          memory[relative_base + param] = value
        else
          raise "Unknown mode #{mode}"
        end
      end
      
      case opcode
      when 1
        a = get_value(memory, memory[pc + 1], modes[0], relative_base)
        b = get_value(memory, memory[pc + 2], modes[1], relative_base)
        set_value(memory, memory[pc + 3], modes[2], relative_base, a + b)
        pc += 4
      when 2
        a = get_value(memory, memory[pc + 1], modes[0], relative_base)
        b = get_value(memory, memory[pc + 2], modes[1], relative_base)
        set_value(memory, memory[pc + 3], modes[2], relative_base, a * b)
        pc += 4
      when 3
        set_value(memory, memory[pc + 1], modes[0], relative_base, input.shift)
        pc += 2
      when 4
        output << get_value(memory, memory[pc + 1], modes[0], relative_base)
        pc += 2
      when 5
        a = get_value(memory, memory[pc + 1], modes[0], relative_base)
        b = get_value(memory, memory[pc + 2], modes[1], relative_base)
        pc = a != 0 ? b : pc + 3
      when 6
        a = get_value(memory, memory[pc + 1], modes[0], relative_base)
        b = get_value(memory, memory[pc + 2], modes[1], relative_base)
        pc = a == 0 ? b : pc + 3
      when 7
        a = get_value(memory, memory[pc + 1], modes[0], relative_base)
        b = get_value(memory, memory[pc + 2], modes[1], relative_base)
        set_value(memory, memory[pc + 3], modes[2], relative_base, a < b ? 1 : 0)
        pc += 4
      when 8
        a = get_value(memory, memory[pc + 1], modes[0], relative_base)
        b = get_value(memory, memory[pc + 2], modes[1], relative_base)
        set_value(memory, memory[pc + 3], modes[2], relative_base, a == b ? 1 : 0)
        pc += 4
      when 9
        a = get_value(memory, memory[pc + 1], modes[0], relative_base)
        relative_base += a
        pc += 2
      when 99
        break
      else
        raise "Unknown opcode #{opcode}"
      end
    end
    
    output
  end
  
  def count_block_tiles(filename)
    program = read_program(filename)
    output = intcode_program(program)
    
    # Group the output into x, y, and tile_id triplets
    grid = Hash.new(0)
    output.each_slice(3) do |x, y, tile_id|
      grid[[x, y]] = tile_id
    end
  
    # Count block tiles (tile_id = 2)
    block_count = grid.values.count(2)
    block_count
  end
  
  
  filename = 'input_level_13.txt'
puts "Number of block tiles: #{count_block_tiles(filename)}"