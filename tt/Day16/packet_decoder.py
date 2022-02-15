import argparse

packet_metadata = []


def load_data(file_name):
    with open(file_name, 'r') as f:
        return f.readline()


def decode_packet(encoded_binary_data):
    if encoded_binary_data == '':
        return 0
    version = encoded_binary_data[:3]
    packet_type = encoded_binary_data[3:6]
    length_mode = '2'
    packet_data_length = 0

    if packet_type != '100':
        length_mode = encoded_binary_data[6]
        if length_mode == '0':
            packet_data_length = int(encoded_binary_data[7:22])
            temp_packet_data_length = packet_data_length
            while temp_packet_data_length:
                length_of_child_packet = decode_packet(encoded_binary_data[22+packet_data_length-temp_packet_data_length:])
                temp_packet_data_length = temp_packet_data_length - length_of_child_packet
            packet_data = encoded_binary_data[22:22+packet_data_length]
            decode_packet(encoded_binary_data[22+packet_data_length:])
            packet_length = packet_data_length + 22
        elif length_mode == '1':
            packet_data_length = int(encoded_binary_data[7:18])
            temp_packet_data_length = packet_data_length
            current_index = 18

            while temp_packet_data_length:
                length_of_child_packet = decode_packet(encoded_binary_data[current_index:])
                current_index = current_index + length_of_child_packet
                temp_packet_data_length = temp_packet_data_length - 1
            packet_data = encoded_binary_data[18:18 + packet_data_length]
            decode_packet(encoded_binary_data[18 + packet_data_length:])
            packet_length = packet_data_length + 18

        packet_metadata.append((version, packet_type, length_mode, packet_data_length, packet_data))
    elif packet_type == '100':
        i = 0
        packet_data = ''
        while encoded_binary_data[6+i*5] != 0:
            packet_data = packet_data + encoded_binary_data[6+i*5:6+(i+1)*5]
            i = i + 1
        packet_data = packet_data + encoded_binary_data[6 + i * 5:6 + (i + 1) * 5]
        packet_metadata.append(version, packet_type, length_mode, packet_data_length, packet_data)
    else:
        print("Packet type invalid. Please check.")
        exit(1)

    return packet_length


def sum_version_numbers(encoded_hex_data):
    version_sum = 0
    encoded_binary_data = ''
    for hex_char in encoded_hex_data:
        encoded_binary_data = encoded_binary_data + bin(int(hex_char, 16))[2:].zfill(4)

    decode_packet(encoded_binary_data)

    for packet in packet_metadata:
        version_sum = version_sum + packet[0]

    return version_sum


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
    print(encoded_hex_data)
    if arguments.code == 1:
        print(sum_version_numbers(encoded_hex_data))
    # elif arguments.code == 2:
    #     dots_after_all_folds(coordinates, fold_instructions)
    # else:
    #     print("Selected code not valid")


if __name__ == "__main__":
    main()
