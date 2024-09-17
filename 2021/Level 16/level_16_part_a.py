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
    version_sum = version
    
    if type_id == 4:  # Literal value
        value, start_idx = parse_literal(binary, start_idx)
    else:  # Operator packet
        length_type_id = binary[start_idx]
        start_idx += 1
        subpacket_version_sum = 0
        
        if length_type_id == '0':
            subpacket_length = int(binary[start_idx:start_idx + 15], 2)
            start_idx += 15
            end_idx = start_idx + subpacket_length
            while start_idx < end_idx:
                sub_version_sum, start_idx = parse_packet(binary, start_idx)
                subpacket_version_sum += sub_version_sum
        else:
            subpacket_count = int(binary[start_idx:start_idx + 11], 2)
            start_idx += 11
            for _ in range(subpacket_count):
                sub_version_sum, start_idx = parse_packet(binary, start_idx)
                subpacket_version_sum += sub_version_sum
        
        version_sum += subpacket_version_sum
    
    return version_sum, start_idx

def sum_versions(hex_input):
    binary = hex_to_bin(hex_input)
    version_sum, _ = parse_packet(binary, 0)
    return version_sum

if __name__ == "__main__":
    # Read the input from the txt file
    with open("input_level_16.txt") as file:
        hex_input = file.read().strip()

    # Calculate the sum of the version numbers in all packets
    result = sum_versions(hex_input)
    print(f"Sum of the version numbers: {result}")
