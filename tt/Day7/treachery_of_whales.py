import argparse


def load_data(file_name):
    fish_spawn_routine = [0] * 9
    with open(file_name, 'r') as f:
        data = list(map(int, f.readline().split(',')))
    for number in data:
        fish_spawn_routine[number] += 1
    return fish_spawn_routine




def main():
    parser = argparse.ArgumentParser(description='AoC Day 5 Vent Danger spots')
    parser.add_argument('-f', '--file',
                        help='Input file with each line containing start and end points of a line representing'
                             'thermal vents',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Horizontal and Vertical lines only or 2: Diagonal lines too',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    lines, max_x_coordinate, max_y_coordinate = load_data(arguments.file)

    if arguments.code == 1:
        print(count_danger_points(1, lines, max_x_coordinate, max_y_coordinate))
    # elif arguments.code == 2:
    #     print(count_danger_points(2, lines, max_x_coordinate, max_y_coordinate))
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()