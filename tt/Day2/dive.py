import argparse


def load_data(file_name):
    data = []
    with open(file_name, 'r') as f:
        for line in f:
            record = line.split()
            data.append((record[0], int(record[1])))

    return data


def aim_and_move(moves_list):
    x = 0
    y = 0
    aim = 0
    for move in moves_list:
        if move[0] == 'forward':
            x += move[1]
            y += aim * move[1]
        if move[0] == 'up':
            aim -= move[1]
        if move[0] == 'down':
            aim += move[1]
    return x, y


def move(moves_list):
    x = 0
    y = 0
    for move in moves_list:
        if move[0] == 'forward':
            x += move[1]
        if move[0] == 'up':
            y -= move[1]
        if move[0] == 'down':
            y += move[1]
    return x, y


def main():
    parser = argparse.ArgumentParser(description='AoC Day 1 Sonar Sweeper')
    parser.add_argument('-f', '--file',
                        help='Input file containing integer values on each line representing depths',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Move or 2: Aim and move',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    moves_list = load_data(arguments.file)
    if arguments.code == 1:
        (x, y) = move(moves_list)
    elif arguments.code == 2:
        (x, y) = aim_and_move(moves_list)
    else:
        print("Selected code not valid")

    print('x: ' + str(x))
    print('-y: ' + str(y))
    print('position: ' + str(x * y))


if __name__ == "__main__":
    main()
