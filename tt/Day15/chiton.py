import argparse
from colorama import init, Fore, Back, Style

MAXINT = 9999999


def load_data(file_name):
    data = []
    with open(file_name, 'r') as f:
        for line in f:
            numbers = []
            for char in line.strip():
                numbers.append([int(char), MAXINT])
            data.append(numbers)

    return data


def print_grid(grid, index, m=-1, n=-1):
    for i in range(len(grid)):
        print()
        for j in range(len(grid[i])):
            if m==i and n==j:
                print(Fore.GREEN + str(grid[i][j][index]).center(4), end='')
                print(",", end='')
            else:
                print(str(grid[i][j][index]).center(4)+",", end='')

    print()


def print_grid_path(grid):
    path_grid = []
    for i in range(len(grid)):
        path_grid.append([])
        for j in range(len(grid)):
            path_grid[i].append(0)

    i=len(grid)-1
    j=len(grid[0])-1
    path_grid[i][j]=1
    while path_grid[0][0]!=1:
        if j>0 and grid[i][j][1]==grid[i][j-1][1]+grid[i][j][0]:
            path_grid[i][j-1]=1
            j=j-1
        elif j<len(grid[i])-1 and grid[i][j][1]==grid[i][j+1][1]+grid[i][j][0]:
            path_grid[i][j+1]=1
            j=j+1
        elif i>0 and grid[i][j][1]==grid[i-1][j][1]+grid[i][j][0]:
            path_grid[i-1][j]=1
            i=i-1
        elif j<len(grid)-1 and grid[i][j][1]==grid[i+1][j][1]+grid[i][j][0]:
            path_grid[i+1][j]=1
            i=i+1
        else:
            print("Examine: (", i, ",", j, ")")
            print_grid(grid, 0, i, j)
            print_grid(grid, 1, i, j)
            break

    for i in range(len(path_grid)):
        print()
        for j in range(len(path_grid[i])):
            if path_grid[i][j]:
                print(Fore.GREEN + str(path_grid[i][j]), end='')
            else:
                print(path_grid[i][j], end='')
    print()


def calculate_lowest_risk_level(grid):
    grid[0][0] = [0, 0]
    coordinates_to_evaluate = [(0, 0)]
    count = 0
    while coordinates_to_evaluate:
        count = count + 1
        coordinates_to_evaluate.sort(key=lambda k:k[1])
        (y, x) = coordinates_to_evaluate.pop(0)
        if x > 0 and grid[y][x-1][1] > grid[y][x][1] + grid[y][x-1][0]:
            grid[y][x-1][1] = grid[y][x][1] + grid[y][x-1][0]
            if (y, x-1) not in coordinates_to_evaluate:
                coordinates_to_evaluate.append((y, x-1))
        if x < len(grid[y]) - 1 and grid[y][x+1][1] > grid[y][x][1] + grid[y][x+1][0]:
            grid[y][x+1][1] = grid[y][x][1] + grid[y][x+1][0]
            if (y, x+1) not in coordinates_to_evaluate:
                coordinates_to_evaluate.append((y, x+1))
        if y > 0 and grid[y-1][x][1] > grid[y][x][1] + grid[y-1][x][0]:
            grid[y-1][x][1] = grid[y][x][1] + grid[y-1][x][0]
            if (y-1, x) not in coordinates_to_evaluate:
                coordinates_to_evaluate.append((y-1, x))
        if y < len(grid) - 1 and grid[y+1][x][1] > grid[y][x][1] + grid[y+1][x][0]:
            grid[y+1][x][1] = grid[y][x][1] + grid[y+1][x][0]
            if (y+1, x) not in coordinates_to_evaluate:
                coordinates_to_evaluate.append((y+1, x))

    print(count)
    # print_grid(grid, 0)
    # print()
    # print_grid(grid, 1)
    # print()
    print_grid_path(grid)
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
    init(autoreset=True)
    grid = load_data(arguments.file)
    if arguments.code == 1:
        print(calculate_lowest_risk_level(grid))
    elif arguments.code == 2:
        print(calculate_lowest_risk_level_extended_grid(grid))
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
