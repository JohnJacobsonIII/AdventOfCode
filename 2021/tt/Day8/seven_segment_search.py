import argparse


def load_data(file_name):
    data1 = []
    data2 = []
    with open(file_name, 'r') as f:
        for line in f:
            # data1.append(line.split()[:-1])
            # data2.append(f.readline().strip().split())
            data = line.split('|')
            data1.append(data[0].split())
            data2.append(data[1].split())
    return data1, data2


def unique_patterned_output_digits(output_entries):
    count = 0
    for output_entry in output_entries:
        for digit in output_entry:
            num_list_wires = len(digit)
            if num_list_wires == 2 or num_list_wires == 3 or num_list_wires == 4 or num_list_wires == 7:
                count = count + 1
    return count


def deduce_digit_patterns(signal_pattern):
    digits_identified = 0
    digit_mappings = {}
    for i in range(len(signal_pattern)):
        num_list_wires = len(signal_pattern[i-digits_identified])
        if num_list_wires == 2:
            digit_mappings[1] = signal_pattern[i-digits_identified]
            signal_pattern.remove(signal_pattern[i-digits_identified])
            digits_identified = digits_identified + 1
        elif num_list_wires == 3:
            digit_mappings[7] = signal_pattern[i-digits_identified]
            signal_pattern.remove(signal_pattern[i-digits_identified])
            digits_identified = digits_identified + 1
        elif num_list_wires == 4:
            digit_mappings[4] = signal_pattern[i-digits_identified]
            signal_pattern.remove(signal_pattern[i-digits_identified])
            digits_identified = digits_identified + 1
        elif num_list_wires == 7:
            digit_mappings[8] = signal_pattern[i-digits_identified]
            signal_pattern.remove(signal_pattern[i-digits_identified])
            digits_identified = digits_identified + 1

    digits_identified = 0
    for i in range(len(signal_pattern)):
        num_list_wires = len(signal_pattern[i-digits_identified])
        if num_list_wires == 5 and digit_mappings[1][0] in signal_pattern[i-digits_identified] and digit_mappings[1][1] in signal_pattern[i-digits_identified]:
            digit_mappings[3] = signal_pattern[i-digits_identified]
            signal_pattern.remove(signal_pattern[i-digits_identified])
            digits_identified = digits_identified + 1
        elif num_list_wires == 5:
            num_segments_matched = 0
            if digit_mappings[4][0] in signal_pattern[i-digits_identified]:
                num_segments_matched = num_segments_matched + 1
            if digit_mappings[4][1] in signal_pattern[i-digits_identified]:
                num_segments_matched = num_segments_matched + 1
            if digit_mappings[4][2] in signal_pattern[i-digits_identified]:
                num_segments_matched = num_segments_matched + 1
            if digit_mappings[4][3] in signal_pattern[i-digits_identified]:
                num_segments_matched = num_segments_matched + 1
            if num_segments_matched == 2:
                digit_mappings[2] = signal_pattern[i-digits_identified]
                signal_pattern.remove(signal_pattern[i-digits_identified])
                digits_identified = digits_identified + 1
        elif num_list_wires == 6 and digit_mappings[1][0] not in signal_pattern[i-digits_identified] or digit_mappings[1][1] not in signal_pattern[i-digits_identified]:
            digit_mappings[6] = signal_pattern[i-digits_identified]
            signal_pattern.remove(signal_pattern[i-digits_identified])
            digits_identified = digits_identified + 1
        elif num_list_wires == 6 and digit_mappings[4][0] in signal_pattern[i-digits_identified] and digit_mappings[4][1] in signal_pattern[i-digits_identified] and \
                digit_mappings[4][2] in signal_pattern[i-digits_identified] and digit_mappings[4][3] in signal_pattern[i-digits_identified]:
            digit_mappings[9] = signal_pattern[i-digits_identified]
            signal_pattern.remove(signal_pattern[i-digits_identified])
            digits_identified = digits_identified + 1

    digits_identified = 0
    for i in range(len(signal_pattern)):
        num_list_wires = len(signal_pattern[i-digits_identified])
        if num_list_wires == 5:
            digit_mappings[5] = signal_pattern[i-digits_identified]
            signal_pattern.remove(signal_pattern[i-digits_identified])
            digits_identified = digits_identified + 1
        elif num_list_wires == 6:
            digit_mappings[0] = signal_pattern[i-digits_identified]
            signal_pattern.remove(signal_pattern[i-digits_identified])
            digits_identified = digits_identified + 1

    return digit_mappings


def get_output_number(digit_mappings, output_entry):
    digits_list = []
    for output_digit in output_entry:
        for (number, pattern) in digit_mappings.items():
            if len(output_digit) == len(pattern):
                num_identified = True
                for wire in output_digit:
                    if wire not in pattern:
                        num_identified = False
                if num_identified:
                    digits_list.append(number)
                    break
    number = digits_list[0] * 1000 + digits_list[1] * 100 + digits_list[2] * 10 + digits_list[3]
    return number


def sum_output_values(signal_patterns, output_entries):
    total = 0
    for (signal_pattern, output_entry) in zip(signal_patterns, output_entries):
        digit_mappings = deduce_digit_patterns(signal_pattern)
        total = total + get_output_number(digit_mappings, output_entry)
    return total


def main():
    parser = argparse.ArgumentParser(description='AoC Day 8 Seven Segment Search')
    parser.add_argument('-f', '--file',
                        help='Input file with pairs of lines, where the first line has signal patterns for the digits,'
                             'and the second line contains four ouput digits following the signal pattern.',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Counting unique digits or 2: Deducing signals and finding total',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    (signal_patterns, output_entries) = load_data(arguments.file)
    # print(signal_patterns)
    # print(output_entries)
    if arguments.code == 1:
        print(unique_patterned_output_digits(output_entries))
    elif arguments.code == 2:
        print(sum_output_values(signal_patterns, output_entries))
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
