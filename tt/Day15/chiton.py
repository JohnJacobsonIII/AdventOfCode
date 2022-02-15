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

    data[0][0] = [0, 0, False]

    return data

def print_grid(grid):
    for i in range(len(grid)):
        number_list = []
        for j in range(len(grid[i])):
            number_list.append(grid[i][j][1])
        print(number_list)

def calculate_lowest_risk_level(grid):
    coordinates_to_evaluate = [(0, 0)]
    count = 0
    while coordinates_to_evaluate:
        count = count + 1
        (y, x) = coordinates_to_evaluate.pop(0)
        if x > 0 and not grid[y][x-1][2] and grid[y][x-1][1] > grid[y][x][1] + grid[y][x-1][0]:
            grid[y][x-1][1] = grid[y][x][1] + grid[y][x-1][0]
            if (y, x-1) not in coordinates_to_evaluate:
                coordinates_to_evaluate.append((y, x-1))
        if x < len(grid[y]) - 1 and not grid[y][x+1][2] and grid[y][x+1][1] > grid[y][x][1] + grid[y][x+1][0]:
            grid[y][x+1][1] = grid[y][x][1] + grid[y][x+1][0]
            if (y, x+1) not in coordinates_to_evaluate:
                coordinates_to_evaluate.append((y, x+1))
        if y > 0 and not grid[y-1][x][2] and grid[y-1][x][1] > grid[y][x][1] + grid[y-1][x][0]:
            grid[y-1][x][1] = grid[y][x][1] + grid[y-1][x][0]
            if (y-1, x) not in coordinates_to_evaluate:
                coordinates_to_evaluate.append((y-1, x))
        if y < len(grid) - 1 and not grid[y+1][x][2] and grid[y+1][x][1] > grid[y][x][1] + grid[y+1][x][0]:
            grid[y+1][x][1] = grid[y][x][1] + grid[y+1][x][0]
            if (y+1, x) not in coordinates_to_evaluate:
                coordinates_to_evaluate.append((y+1, x))
        grid[y][x][2] = True
    print(count)
    return grid[len(grid)-1][len(grid[len(grid)-1])-1][1]


def main():
    parser = argparse.ArgumentParser(description='AoC Day 15 Chiton')
    parser.add_argument('-f', '--file',
                        help='Input file with a number grid representing risk level in cave.',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1:  or 2: .',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    grid = load_data(arguments.file)
    # print(grid)
    if arguments.code == 1:
        print(calculate_lowest_risk_level(grid))
    print_grid(grid)
    # elif arguments.code == 2:
    #     dots_after_all_folds(coordinates, fold_instructions)
    # else:
    #     print("Selected code not valid")


if __name__ == "__main__":
    main()
