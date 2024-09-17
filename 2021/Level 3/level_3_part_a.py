def calculate_power_consumption(filename: str) -> int:
    # Read the binary numbers from the file
    with open(filename, 'r') as file:
        report = [line.strip() for line in file]

    # Get the number of bits in each binary number
    bit_length = len(report[0])

    # Initialize lists to count the number of 1s in each bit position
    bit_counts = [0] * bit_length

    # Count how many 1s there are in each bit position
    for binary in report:
        for i, bit in enumerate(binary):
            bit_counts[i] += int(bit)  # Add 1 if the bit is '1'

    # Determine the gamma and epsilon rates
    gamma_rate = ""
    epsilon_rate = ""

    half_report_length = len(report) / 2
    for count in bit_counts:
        if count > half_report_length:
            gamma_rate += '1'
            epsilon_rate += '0'
        else:
            gamma_rate += '0'
            epsilon_rate += '1'

    # Convert the gamma and epsilon rates from binary to decimal
    gamma_decimal = int(gamma_rate, 2)
    epsilon_decimal = int(epsilon_rate, 2)

    # Calculate power consumption
    return gamma_decimal * epsilon_decimal

if __name__ == "__main__":
    input_file = 'input_level_3.txt'  # Replace with your input file
    result = calculate_power_consumption(input_file)
    print(f"Power consumption: {result}")
