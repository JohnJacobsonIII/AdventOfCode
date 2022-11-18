import argparse


def load_data(file_name):
    data = []
    with open(file_name, 'r') as f:
        for line in f:
            line_data = []
            for digit in line.strip():
                line_data.append(int(digit))
            data.append(line_data)

    return data


def step_increment(grid, flashes):
    flashed = True
    for i in range(len(grid)):
        for j in range(len(grid[i])):
            grid[i][j] = grid[i][j] + 1

    while flashed:
        flashed = False
        for i in range(len(grid)):
            for j in range(len(grid[i])):
                if grid[i][j] > 9:
                    flashed = True
                    flashes = flashes + 1
                    if i > 0 and j > 0 and grid[i - 1][j - 1] != 0:
                        grid[i - 1][j - 1] = grid[i - 1][j - 1] + 1
                    if i > 0 and grid[i - 1][j] != 0:
                        grid[i - 1][j] = grid[i - 1][j] + 1
                    if i > 0 and j < len(grid[i - 1]) - 1 and grid[i - 1][j + 1] != 0:
                        grid[i - 1][j + 1] = grid[i - 1][j + 1] + 1
                    if j > 0 and grid[i][j - 1] != 0:
                        grid[i][j - 1] = grid[i][j - 1] + 1
                    grid[i][j] = 0
                    if j < len(grid[i]) - 1 and grid[i][j + 1] != 0:
                        grid[i][j + 1] = grid[i][j + 1] + 1
                    if i < len(grid) - 1 and j > 0 and grid[i + 1][j - 1] != 0:
                        grid[i + 1][j - 1] = grid[i + 1][j - 1] + 1
                    if i < len(grid) - 1 and grid[i + 1][j] != 0:
                        grid[i + 1][j] = grid[i + 1][j] + 1
                    if i < len(grid) - 1 and j < len(grid[i + 1]) - 1 and grid[i + 1][j + 1] != 0:
                        grid[i + 1][j + 1] = grid[i + 1][j + 1] + 1

    return flashes


def count_flashes(grid, n):
    flashes = 0

    for num_steps in range(n):
        flashes = step_increment(grid, flashes)

    return flashes


def find_full_flash_step(grid):
    step = 0
    total = 0
    for i in range(len(grid)):
        total = total + sum(grid[i])
    while total:
        step_increment(grid, 0)
        step = step + 1
        total = 0
        for i in range(len(grid)):
            total = total + sum(grid[i])

    return step


def main():
    parser = argparse.ArgumentParser(description='AoC Day 11 Dumbo Octopus')
    parser.add_argument('-f', '--file',
                        help='Input file with each line containing energy level of octopus from 0-9.',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Count number of flashes after n steps or 2: Find step number of full flash',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    grid = load_data(arguments.file)
    # print(grid)
    if arguments.code == 1:
        print(count_flashes(grid, 100))
    elif arguments.code == 2:
        print(find_full_flash_step(grid))
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
