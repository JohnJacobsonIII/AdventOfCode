import argparse


def load_data(file_name):
    with open(file_name, 'r') as f:
        data = list(map(int, f.readline().split(',')))
    return data


def constant_cost_function(i, x):
    return abs(x - i)


def linear_cost_function(i, x):
    return sum(range(abs(x - i) + 1))


def find_fuel_required(horizontal_positions_list, cost_function):
    least_total_distance = sum([cost_function(0, x) for x in horizontal_positions_list])
    optimal_horizontal_position = 0
    for i in range(max(horizontal_positions_list)):
        distance_list = [cost_function(i, x) for x in horizontal_positions_list]
        if least_total_distance > sum(distance_list):
            least_total_distance = sum(distance_list)
            optimal_horizontal_position = i
    return least_total_distance


def main():
    parser = argparse.ArgumentParser(description='AoC Day 7 Treachery of Whales')
    parser.add_argument('-f', '--file',
                        help='Input file with each line containing start and end points of a line representing'
                             'thermal vents',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Constant cost function only or 2: Linear cost function',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    horizontal_positions_list = load_data(arguments.file)
    if arguments.code == 1:
        print(find_fuel_required(horizontal_positions_list, constant_cost_function))
    elif arguments.code == 2:
        print(find_fuel_required(horizontal_positions_list, linear_cost_function))
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
