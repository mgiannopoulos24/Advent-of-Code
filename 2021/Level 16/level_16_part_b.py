def hex_to_bin(hex_string):
    scale = 16  # hexadecimal base
    num_of_bits = 4 * len(hex_string)
    return bin(int(hex_string, scale))[2:].zfill(num_of_bits)

def parse_literal(binary, start_idx):
    value_bits = []
    while True:
        group = binary[start_idx:start_idx + 5]
        value_bits.append(group[1:])  # Append the last 4 bits
        start_idx += 5
        if group[0] == '0':  # If the first bit is 0, it's the last group
            break
    return int(''.join(value_bits), 2), start_idx

def parse_packet(binary, start_idx):
    version = int(binary[start_idx:start_idx + 3], 2)
    type_id = int(binary[start_idx + 3:start_idx + 6], 2)
    start_idx += 6
    
    if type_id == 4:  # Literal value
        value, start_idx = parse_literal(binary, start_idx)
    else:  # Operator packet
        length_type_id = binary[start_idx]
        start_idx += 1
        subpacket_values = []
        
        if length_type_id == '0':
            subpacket_length = int(binary[start_idx:start_idx + 15], 2)
            start_idx += 15
            end_idx = start_idx + subpacket_length
            while start_idx < end_idx:
                value, start_idx = parse_packet(binary, start_idx)
                subpacket_values.append(value)
        else:
            subpacket_count = int(binary[start_idx:start_idx + 11], 2)
            start_idx += 11
            for _ in range(subpacket_count):
                value, start_idx = parse_packet(binary, start_idx)
                subpacket_values.append(value)
        
        # Evaluate the packet value based on the type ID
        if type_id == 0:  # Sum packet
            value = sum(subpacket_values)
        elif type_id == 1:  # Product packet
            value = 1
            for v in subpacket_values:
                value *= v
        elif type_id == 2:  # Minimum packet
            value = min(subpacket_values)
        elif type_id == 3:  # Maximum packet
            value = max(subpacket_values)
        elif type_id == 5:  # Greater than packet
            value = 1 if subpacket_values[0] > subpacket_values[1] else 0
        elif type_id == 6:  # Less than packet
            value = 1 if subpacket_values[0] < subpacket_values[1] else 0
        elif type_id == 7:  # Equal to packet
            value = 1 if subpacket_values[0] == subpacket_values[1] else 0
    
    return value, start_idx

def evaluate_expression(hex_input):
    binary = hex_to_bin(hex_input)
    value, _ = parse_packet(binary, 0)
    return value

if __name__ == "__main__":
    # Read the input from the txt file
    with open("input_level_16.txt") as file:
        hex_input = file.read().strip()

    # Calculate the value of the expression represented by the transmission
    result = evaluate_expression(hex_input)
    print(f"The value of the expression is: {result}")
