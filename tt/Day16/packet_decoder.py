import argparse

# Structure of metadata
# (version, packet_type, length_mode, packet_data_length, result, recursion_level, packet_data)
#
from functools import reduce

packet_metadata = []


def load_data(file_name):
    with open(file_name, 'r') as f:
        return f.readline()

# Decoding the packet
# Get the version number
# Get the type
# If literal
#   For subsequent bits,
#   divide into 5 bit groups.
#   Check the the first bit of each group to check if it is the last group
# else
#   check the length mode
def decode_packet(encoded_binary_data, recursion_level):
    # Base case of recursion: The packet is empty or version is 0.
    # print("Starting level: ", recursion_level)
    # print(encoded_binary_data)
    if encoded_binary_data == '' or int(encoded_binary_data, 2) == 0:
        # print("Returning from level: ", recursion_level)
        return 0,0

    # Getting the header data: version number - first 3 bits, packet type - next 3 bits
    version = int(encoded_binary_data[:3], 2)
    packet_type = int(encoded_binary_data[3:6], 2)

    # Default length mode and packet data length for metadata
    length_mode = '2'
    packet_data_length = 0

    # If packet anything but a literal,
    #   Branch on length mode - next 1 bit
    if packet_type != 4:
        length_mode = encoded_binary_data[6]
        sub_packet_results = []

        # If length mode is the first type
        #   Get the length of the data - next 15 bits
        #   Recurse on the data using the length as a counter
        if length_mode == '0':
            packet_data_length = int(encoded_binary_data[7:22], 2)
            temp_packet_data_length = packet_data_length
            # print("Initial packet data length: ", temp_packet_data_length)
            while temp_packet_data_length:
                length_of_child_packet, result = decode_packet(encoded_binary_data[22+packet_data_length-temp_packet_data_length:], recursion_level+1)
                if length_of_child_packet:
                    sub_packet_results.append(result)
                temp_packet_data_length = temp_packet_data_length - length_of_child_packet
                # print("Length of child: ", length_of_child_packet)
                # print("Temp packet data length: ", temp_packet_data_length)
            packet_data = encoded_binary_data[22:22+packet_data_length]
            packet_length = packet_data_length + 22

        # If length of mode is the second type
        #   Get the number of packets - next 11 bits
        #   Recurse on the data using number of packer
        elif length_mode == '1':
            number_of_sub_packets = int(encoded_binary_data[7:18], 2)
            temp_number_of_sub_packets = number_of_sub_packets
            current_index = 18

            packet_data_length = 0
            # print("Initial number of subpackets: ", temp_number_of_sub_packets)
            while temp_number_of_sub_packets:
                length_of_child_packet, result = decode_packet(encoded_binary_data[current_index:], recursion_level+1)
                if length_of_child_packet:
                    sub_packet_results.append(result)
                current_index = current_index + length_of_child_packet
                temp_number_of_sub_packets = temp_number_of_sub_packets - 1
                packet_data_length += length_of_child_packet
                # print("Length of child:", length_of_child_packet)
                # print("Temp number of subpackets: ", temp_number_of_sub_packets)

            packet_data = encoded_binary_data[18:18+packet_data_length]
            packet_length = packet_data_length + 18

        # Calculating result
        if packet_type == 0:
            result = sum(sub_packet_results)
        elif packet_type == 1:
            result = reduce((lambda x, y: x * y), sub_packet_results)
        elif packet_type == 2:
            result = min(sub_packet_results)
        elif packet_type == 3:
            result = max(sub_packet_results)
        elif packet_type == 5:
            assert len(sub_packet_results) == 2
            result = 1 if sub_packet_results[0] > sub_packet_results[1] else 0
        elif packet_type == 6:
            assert len(sub_packet_results) == 2
            result = 1 if sub_packet_results[0] < sub_packet_results[1] else 0
        elif packet_type == 7:
            assert len(sub_packet_results) == 2
            result = 1 if sub_packet_results[0] == sub_packet_results[1] else 0
        else:
            print("Somethings wrong. Packet type did not match any.")
            exit(1)
    # If packet is a literal packet
    elif packet_type == 4:
        i = 0
        packet_data = ''
        while 6 + i*5 < len(encoded_binary_data) and encoded_binary_data[6 + i*5] != '0':
            packet_data = packet_data + encoded_binary_data[6 + i*5:6 + (i+1)*5]
            i = i + 1
        packet_data = packet_data + encoded_binary_data[6 + i*5:6 + (i+1)*5]
        packet_length = len(packet_data) + 6

        result = ''
        for i in range(i+1):
            result += packet_data[5*i+1:5*(i+1)]
        result = int(result, 2)

        # print("Literal packet length", str(packet_length))
    else:
        print("Packet type invalid. Please check.")
        exit(1)


    packet_metadata.append((version, packet_type, length_mode, packet_data_length, result, recursion_level, packet_data))
    # print(packet_metadata[len(packet_metadata) - 1])

    # print("Returning from level: ", recursion_level)
    return packet_length, result


def sum_version_numbers(encoded_hex_data):
    version_sum = 0
    encoded_binary_data = ''
    for hex_char in encoded_hex_data:
        encoded_binary_data = encoded_binary_data + bin(int(hex_char, 16))[2:].zfill(4)

    decode_packet(encoded_binary_data, 1)
    # for item in packet_metadata:
    #     print(item)

    for packet in packet_metadata:
        version_sum = version_sum + packet[0]

    return version_sum


def result_of_decoding(encoded_hex_data):
    encoded_binary_data = ''
    for hex_char in encoded_hex_data:
        encoded_binary_data = encoded_binary_data + bin(int(hex_char, 16))[2:].zfill(4)

    decode_packet(encoded_binary_data, 1)
    # for item in packet_metadata:
    #     print(item)

    return packet_metadata[len(packet_metadata)-1][4]


def main():
    parser = argparse.ArgumentParser(description='AoC Day 16 Packet Decoder')
    parser.add_argument('-f', '--file',
                        help='Input file with hexadecimal data to be decoded.',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1:  or 2: .',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    encoded_hex_data = load_data(arguments.file)
    # print(encoded_hex_data)
    if arguments.code == 1:
        print(sum_version_numbers(encoded_hex_data))
    elif arguments.code == 2:
        print(result_of_decoding(encoded_hex_data))
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
