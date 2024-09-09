def read_program(filename)
    File.read(filename).strip.split(',').map(&:to_i)
  end
  
  def intcode_program(program, input = [])
    pc = 0
    relative_base = 0
    memory = Hash.new(0)
    program.each_with_index { |val, idx| memory[idx] = val }
    
    memory[0] = 2 # Set free play mode by inserting "2 quarters" in memory address 0
    
    output = []
    
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
    
    paddle_x = 0
    ball_x = 0
    score = 0
    
    while true
      opcode = memory[pc] % 100
      modes = [(memory[pc] / 100) % 10, (memory[pc] / 1000) % 10, (memory[pc] / 10000) % 10]
      
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
        # Joystick input: -1 (left), 0 (neutral), 1 (right)
        if ball_x < paddle_x
          input_value = -1
        elsif ball_x > paddle_x
          input_value = 1
        else
          input_value = 0
        end
        set_value(memory, memory[pc + 1], modes[0], relative_base, input_value)
        pc += 2
      when 4
        output << get_value(memory, memory[pc + 1], modes[0], relative_base)
        if output.size == 3
          x, y, tile_id = output
          output.clear
          if x == -1 && y == 0
            # Update the score
            score = tile_id
          else
            # Update ball and paddle positions
            case tile_id
            when 3
              paddle_x = x
            when 4
              ball_x = x
            end
          end
        end
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
    
    score
  end
  
  def count_block_tiles_and_play(filename)
    program = read_program(filename)
    score = intcode_program(program)
    
    puts "Final score: #{score}"
  end
  
  filename = 'input_level_13.txt'
  count_block_tiles_and_play(filename)
  