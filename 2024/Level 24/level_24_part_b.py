import os

def read_input(file_path):
    """
    Reads the input file and splits it into wires and gates sections.
    
    :param file_path: Path to the input file.
    :return: Tuple containing lists of wires and gates.
    """
    with open(file_path, 'r', encoding='utf-8') as file:
        content = file.read().strip()
        wires_raw, gates_raw = content.split("\n\n")
        return wires_raw.split("\n"), gates_raw.split("\n")

def parse_wires(wires_raw):
    """
    Parses the raw wires data into a dictionary.
    
    :param wires_raw: List of wire definitions in 'name: value' format.
    :return: Dictionary mapping wire names to their integer values.
    """
    wires = {}
    for line in wires_raw:
        name, value = line.split(": ")
        wires[name] = int(value)
    return wires

def parse_gates(gates_raw, wires):
    """
    Parses the raw gates data into a list of gate dictionaries.
    
    :param gates_raw: List of gate definitions in 'inputs -> output' format.
    :param wires: Dictionary of existing wires to initialize new ones as None.
    :return: List of gate dictionaries with keys 'a', 'op', 'b', and 'output'.
    """
    gates = []
    for line in gates_raw:
        inputs, output = line.split(" -> ")
        parts = inputs.split(" ")
        
        if len(parts) == 3:
            a, op, b = parts
        else:
            a, op = parts
            b = None

        gate = {'a': a, 'op': op, 'b': b, 'output': output}
        gates.append(gate)

        # Initialize wires if they don't exist
        for wire in [a, b, output]:
            if wire and wire not in wires:
                wires[wire] = None

    return gates

def is_direct(gate):
    """Checks if either input starts with 'x'."""
    return gate['a'].startswith('x') or (gate['b'] and gate['b'].startswith('x'))

def is_output(gate):
    """Checks if the output starts with 'z'."""
    return gate['output'].startswith('z')

def is_gate(type_op):
    """Returns a function that checks if a gate's operation matches the specified type."""
    return lambda gate: gate['op'] == type_op

def has_output(output):
    """Returns a function that checks if a gate's output matches the specified output."""
    return lambda gate: gate['output'] == output

def has_input(input_wire):
    """Returns a function that checks if a gate's inputs include the specified wire."""
    return lambda gate: gate['a'] == input_wire or gate['b'] == input_wire

def main():
    """
    Main function to read input, parse data, and compute flagged outputs.
    """
    # Read and parse input
    wires_raw, gates_raw = read_input("input_level_24.txt")
    wires = parse_wires(wires_raw)
    input_bit_count = len(wires_raw) // 2
    gates = parse_gates(gates_raw, wires)
    
    # Initialize set for flagged outputs
    flags = set()

    # Filter gates into categories
    FA_GATE0 = list(filter(is_direct, filter(is_gate("XOR"), gates)))
    FA_GATE3 = list(filter(lambda g: g['op'] == "XOR" and not is_direct(g), gates))
    output_gates = list(filter(is_output, gates))
    
    # Check FA Gate0 gates
    for gate in FA_GATE0:
        a, b, output = gate['a'], gate['b'], gate['output']
        is_first = a == "x00" or b == "x00"
        if is_first:
            if output != "z00":
                flags.add(output)
        elif output == "z00":
            flags.add(output)
        elif is_output(gate):
            flags.add(output)

    # Check FA Gate3 gates
    for gate in FA_GATE3:
        if not is_output(gate):
            flags.add(gate['output'])

    # Check output gates
    for gate in output_gates:
        is_last = gate['output'] == f"z{str(input_bit_count).zfill(2)}"
        if is_last:
            if gate['op'] != "OR":
                flags.add(gate['output'])
        elif gate['op'] != "XOR":
            flags.add(gate['output'])

    # Additional checks for FA Gate0
    check_next = []
    for gate in FA_GATE0:
        output = gate['output']
        if output in flags or output == "z00":
            continue
        
        matches = list(filter(has_input(output), FA_GATE3))
        if not matches:
            check_next.append(gate)
            flags.add(output)

    for gate in check_next:
        a, output = gate['a'], gate['output']
        intended_result = f"z{a[1:]}"
        matches = list(filter(has_output(intended_result), FA_GATE3))
        if len(matches) != 1:
            raise Exception("Wrong input.")
        
        match = matches[0]
        to_check = [match['a'], match['b']]
        or_matches = list(filter(is_gate("OR"), gates))
        or_matches = list(filter(lambda g: g['output'] in to_check, or_matches))
        
        if len(or_matches) != 1:
            raise Exception("Too complex.")
        
        or_match_output = or_matches[0]['output']
        correct_output = next((wire for wire in to_check if wire != or_match_output), None)
        flags.add(correct_output)

    # Final validation
    if len(flags) != 8:
        raise Exception("Too complex.")
    
    print(",".join(sorted(flags)))

if __name__ == "__main__":
    main()
