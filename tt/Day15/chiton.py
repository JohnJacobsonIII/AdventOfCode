import argparse

MAXINT = 9999999


def load_data(file_name):
    data = []
    with open(file_name, 'r') as f:
        for line in f:
            numbers = []
            for char in line.strip():
                numbers.append([int(char), MAXINT, False])
            data.append(numbers)

    return data


def print_grid(grid, index):
    for i in range(len(grid)):
        number_list = []
        for j in range(len(grid[i])):
            number_list.append(grid[i][j][index])
        print(number_list)


def calculate_lowest_risk_level(grid):
    grid[0][0] = [0, 0, False]
    coordinates_to_evaluate = [(0, 0)]
    count = 0
    while coordinates_to_evaluate:
        count = count + 1
        (y, x) = coordinates_to_evaluate.pop(0)
        if x > 0 and grid[y][x-1][1] > grid[y][x][1] + grid[y][x-1][0]:
            grid[y][x-1][1] = grid[y][x][1] + grid[y][x-1][0]
            if (y, x-1) not in coordinates_to_evaluate and not grid[y][x-1][2]:
                coordinates_to_evaluate.append((y, x-1))
        if x < len(grid[y]) - 1 and grid[y][x+1][1] > grid[y][x][1] + grid[y][x+1][0]:
            grid[y][x+1][1] = grid[y][x][1] + grid[y][x+1][0]
            if (y, x+1) not in coordinates_to_evaluate  and not grid[y][x+1][2]:
                coordinates_to_evaluate.append((y, x+1))
        if y > 0 and grid[y-1][x][1] > grid[y][x][1] + grid[y-1][x][0]:
            grid[y-1][x][1] = grid[y][x][1] + grid[y-1][x][0]
            if (y-1, x) not in coordinates_to_evaluate and not grid[y-1][x][2]:
                coordinates_to_evaluate.append((y-1, x))
        if y < len(grid) - 1 and grid[y+1][x][1] > grid[y][x][1] + grid[y+1][x][0]:
            grid[y+1][x][1] = grid[y][x][1] + grid[y+1][x][0]
            if (y+1, x) not in coordinates_to_evaluate and not grid[y+1][x][2]:
                coordinates_to_evaluate.append((y+1, x))
        grid[y][x][2] = True
    print(count)
    return grid[len(grid)-1][len(grid[len(grid)-1])-1][1]


def calculate_lowest_risk_level_extended_grid(grid):
    extended_grid = []
    for i in range(len(grid)*5):
        extended_grid.append([])
        for j in range(len(grid)*5):
            extended_grid[i].append([0, MAXINT, False])

    for i in range(len(grid)):
        for j in range(len(grid[i])):
            for m in range(5):
                for n in range(5):
                    if grid[i][j][0]+m+n < 10:
                        extended_grid[len(grid)*m + i][len(grid[i])*n + j][0] = (grid[i][j][0]+m+n) % 10
                    else:
                        extended_grid[len(grid)*m + i][len(grid[i])*n + j][0] = (grid[i][j][0]+m+n) % 10 + 1
                    extended_grid[len(grid)*m + i][len(grid[i])*n + j][1] = MAXINT

    # print_grid(extended_grid, 0)

    return calculate_lowest_risk_level(extended_grid)


def main():
    parser = argparse.ArgumentParser(description='AoC Day 15 Chiton')
    parser.add_argument('-f', '--file',
                        help='Input file with a number grid representing risk level in cave.',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Find lowest risk level or 2: Find lowest risk level in extended grid.',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    grid = load_data(arguments.file)
    # print_grid_with_path(grid)
    if arguments.code == 1:
        print(calculate_lowest_risk_level(grid))
    elif arguments.code == 2:
        print(calculate_lowest_risk_level_extended_grid(grid))
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
