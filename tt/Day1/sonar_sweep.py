import argparse


def load_data(file_name):
    data = []
    with open(file_name, 'r') as f:
        for line in f:
            data.append(int(line.strip()))

    return data


def three_measurement_sliding_window(integer_list):
    sum_list = [integer_list[i - 1] + integer_list[i] + integer_list[i + 1] for i in
                range(1, len(integer_list) - 1)]
    count = count_increments(sum_list)

    return count


def count_increments(integer_list):
    count = 0

    for i in range(1, len(integer_list)):
        if integer_list[i] > integer_list[i - 1]:
            count += 1

    return count


def main():
    parser = argparse.ArgumentParser(description='AoC Day 1 Sonar Sweeper')
    parser.add_argument('-f', '--file',
                        help='Input file containing integer values on each line representing depths',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Count increments method or 2: Three-measurement sliding window method',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    depths_list = load_data(arguments.file)
    if arguments.code == 1:
        print(count_increments(depths_list))
    elif arguments.code == 2:
        print(three_measurement_sliding_window(depths_list))
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
