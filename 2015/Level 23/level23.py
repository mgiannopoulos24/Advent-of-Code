def run_program(instructions):
    # Initialize registers a and b
    registers = {'a': 0, 'b': 0}
    # Initialize instruction pointer
    ip = 0

    while ip < len(instructions):
        instruction = instructions[ip].split()

        if instruction[0] == 'hlf':
            registers[instruction[1]] //= 2
            ip += 1
        elif instruction[0] == 'tpl':
            registers[instruction[1]] *= 3
            ip += 1
        elif instruction[0] == 'inc':
            registers[instruction[1]] += 1
            ip += 1
        elif instruction[0] == 'jmp':
            ip += int(instruction[1])
        elif instruction[0] == 'jie':
            if registers[instruction[1][0]] % 2 == 0:
                ip += int(instruction[2])
            else:
                ip += 1
        elif instruction[0] == 'jio':
            if registers[instruction[1][0]] == 1:
                ip += int(instruction[2])
            else:
                ip += 1

    return registers['b']

# Define the input program
program = [
    "jio a, +19",
    "inc a",
    "tpl a",
    "inc a",
    "tpl a",
    "inc a",
    "tpl a",
    "tpl a",
    "inc a",
    "inc a",
    "tpl a",
    "tpl a",
    "inc a",
    "inc a",
    "tpl a",
    "inc a",
    "inc a",
    "tpl a",
    "jmp +23",
    "tpl a",
    "tpl a",
    "inc a",
    "inc a",
    "tpl a",
    "inc a",
    "inc a",
    "tpl a",
    "inc a",
    "tpl a",
    "inc a",
    "tpl a",
    "inc a",
    "tpl a",
    "inc a",
    "inc a",
    "tpl a",
    "inc a",
    "inc a",
    "tpl a",
    "tpl a",
    "inc a",
    "jio a, +8",
    "inc b",
    "jie a, +4",
    "tpl a",
    "inc a",
    "jmp +2",
    "hlf a",
    "jmp -7"
]

# Run the program and print the value in register b
print("Value in register b:", run_program(program))
