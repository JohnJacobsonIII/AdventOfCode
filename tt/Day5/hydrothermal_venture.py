import argparse


def load_data(file_name):
    lines = []
    max_x = 0
    max_y = 0
    with open(file_name, 'r') as f:
        for line in f:
            start_point = line.strip().split()[0].split(',')
            end_point = line.strip().split()[2].split(',')
            if max_x < int(start_point[0]):
                max_x = int(start_point[0])
            if max_x < int(end_point[0]):
                max_x = int(end_point[0])
            if max_y < int(start_point[1]):
                max_y = int(start_point[1])
            if max_y < int(end_point[1]):
                max_y = int(end_point[1])
            lines.append(((int(start_point[0]), int(start_point[1])), (int(end_point[0]), int(end_point[1]))))
    # print(lines)
    # print(max_x_coordinate, max_y_coordinate)

    return lines, max_x + 1, max_y + 1


def horizontal_vertical_vents(line, grid):
    x1 = line[0][0]
    y1 = line[0][1]
    x2 = line[1][0]
    y2 = line[1][1]

    if x1 == x2:
        y1 = min(line[0][1], line[1][1])
        y2 = max(line[0][1], line[1][1])
        for y in range(y1, y2 + 1):
            grid[y][x1] += 1
    elif y1 == y2:
        x1 = min(line[0][0], line[1][0])
        x2 = max(line[0][0], line[1][0])
        for x in range(x1, x2 + 1):
            grid[y1][x] += 1
    return


def diagonal_vents(line, grid):
    x1 = line[0][0]
    y1 = line[0][1]
    x2 = line[1][0]
    y2 = line[1][1]

    if x1 < x2:
        if y1 < y2:
            for x, y in zip(range(x1, x2 + 1), range(y1, y2 + 1)):
                grid[y][x] += 1
        else:
            for x, y in zip(range(x1, x2 + 1), range(y1, y2 - 1, -1)):
                grid[y][x] += 1
    else:
        if y1 < y2:
            for x, y in zip(range(x1, x2 - 1, -1), range(y1, y2 + 1)):
                grid[y][x] += 1
        else:
            for x, y in zip(range(x1, x2 - 1, -1), range(y1, y2 - 1, -1)):
                grid[y][x] += 1

    return


def count_danger_points(code, lines, max_x_coordinate, max_y_coordinate):
    count = 0
    grid = []
    for i in range(0, max_y_coordinate):
        grid.append([0] * max_x_coordinate)

    # print(grid)
    # print(lines)
    for line in lines:
        if line[0][0] == line[1][0] or line[0][1] == line[1][1]:
            horizontal_vertical_vents(line, grid)
        elif abs(line[0][0] - line[1][0]) == abs(line[0][1] - line[1][1]) and code == 2:
            diagonal_vents(line, grid)
    # print(grid)

    for row in grid:
        for elem in row:
            if elem > 1:
                count += 1

    return count


def main():
    parser = argparse.ArgumentParser(description='AoC Day 1 Sonar Sweeper')
    parser.add_argument('-f', '--file',
                        help='Input file containing integer values on each line representing depths',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Horizontal and Vertical lines only or 2: Diagonal lines too',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    lines, max_x_coordinate, max_y_coordinate = load_data(arguments.file)

    if arguments.code == 1:
        print(count_danger_points(1, lines, max_x_coordinate, max_y_coordinate))
    elif arguments.code == 2:
        print(count_danger_points(2, lines, max_x_coordinate, max_y_coordinate))
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
