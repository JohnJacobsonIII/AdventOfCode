import argparse


def load_data(file_name):
    data = []
    with open(file_name, 'r') as f:
        for line in f:
            data.append(line.strip())

    return data


def count_bits(bin_number_list):
    counts = [0] * len(bin_number_list)

    for bin_number in bin_number_list:
        for i in range(len(bin_number)):
            if bin_number[i] == '0':
                counts[i] += 1

    gamma = ['0' if counts[i] > len(bin_number_list) / 2 else '1' for i in range(len(bin_number_list[0]))]
    epsilon = ['1' if counts[i] > len(bin_number_list) / 2 else '0' for i in range(len(bin_number_list[0]))]

    gamma = int(''.join(gamma), 2)
    epsilon = int(''.join(epsilon), 2)

    return gamma, epsilon


def criteria_gamma(bin_number_list):
    candidate_list = bin_number_list

    for i in range(len(bin_number_list[0])):
        if len(candidate_list) > 1:
            zeros_list = []
            ones_list = []
            for bin_number in candidate_list:
                zeros_list.append(bin_number) if bin_number[i] == '0' else ones_list.append(bin_number)
            if len(zeros_list) > len(ones_list):
                candidate_list = zeros_list
            else:
                candidate_list = ones_list
        else:
            break

    gamma = int(candidate_list[0], 2)

    return gamma


def criteria_epsilon(bin_number_list):
    candidate_list = bin_number_list

    for i in range(len(bin_number_list[0])):
        if len(candidate_list) > 1:
            zeros_list = []
            ones_list = []
            for bin_number in candidate_list:
                zeros_list.append(bin_number) if bin_number[i] == '0' else ones_list.append(bin_number)
            if len(zeros_list) <= len(ones_list):
                candidate_list = zeros_list
            else:
                candidate_list = ones_list
        else:
            break

    epsilon = int(candidate_list[0], 2)

    return epsilon


def main():
    parser = argparse.ArgumentParser(description='AoC Day 3 Rate calculation')
    parser.add_argument('-f', '--file',
                        help='Input file with each line containing binary numbers.',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: count bits or 2: Bit criteria',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    bin_number_list = load_data(arguments.file)

    if arguments.code == 1:
        (gamma, epsilon) = count_bits(bin_number_list)

    elif arguments.code == 2:
        gamma = criteria_gamma(bin_number_list)
        epsilon = criteria_epsilon(bin_number_list)
    else:
        print("Selected code not valid")

    print(gamma * epsilon)


if __name__ == "__main__":
    main()
