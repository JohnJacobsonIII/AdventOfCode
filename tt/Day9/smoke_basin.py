import argparse
import sys
from collections import defaultdict

sys.setrecursionlimit(10**6)

def load_data(file_name):
    data = []
    with open(file_name, 'r') as f:
        for line in f:
            line_data = []
            for digit in line.strip():
                line_data.append(int(digit))
            data.append(line_data)

    return data


def is_low_point(height_map, i, j):
    if i > 0 and height_map[i][j] >= height_map[i - 1][j]:
        return False
    if i < len(height_map) - 1 and height_map[i][j] >= height_map[i + 1][j]:
        return False
    if j > 0 and height_map[i][j] >= height_map[i][j - 1]:
        return False
    if j < len(height_map[0]) - 1 and height_map[i][j] >= height_map[i][j + 1]:
        return False
    return True


def calculate_total_risk(height_map):
    total_risk = 0

    for i in range(len(height_map)):
        for j in range(len(height_map[i])):
            if is_low_point(height_map, i, j):
                total_risk = total_risk + height_map[i][j] + 1

    return total_risk


def find_basin(height_map, i, j):
    m = i
    n = j
    while not is_low_point(height_map, m, n):
        if m > 0 and height_map[m][n] > height_map[m - 1][n]:
            m = m-1
        elif m < len(height_map) - 1 and height_map[m][n] > height_map[m + 1][n]:
            m = m + 1
        elif n > 0 and height_map[m][n] > height_map[m][n - 1]:
            n = n - 1
        elif n < len(height_map[0]) - 1 and height_map[m][n] > height_map[m][n + 1]:
            n = n + 1
    return m, n


def product_top_three_basins(height_map):
    basin_size_dict = defaultdict(lambda: 0)
    for i in range(len(height_map)):
        for j in range(len(height_map[i])):
            if height_map[i][j] != 9:
                low_point = find_basin(height_map, i, j)
                basin_size_dict[low_point] = basin_size_dict[low_point] + 1
    basin_sizes = list(basin_size_dict.values())
    basin_sizes.sort(reverse=True)
    return basin_sizes[0] * basin_sizes[1] * basin_sizes[2]


def main():
    parser = argparse.ArgumentParser(description='AoC Day 9 Smoke Basin')
    parser.add_argument('-f', '--file',
                        help='Input file with each line containing numbers representing height of floor.',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Calculate total Risk level or 2: Multiply 3 largest basins',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    height_map = load_data(arguments.file)
    # print(height_map)
    if arguments.code == 1:
        print(calculate_total_risk(height_map))
    elif arguments.code == 2:
        print(product_top_three_basins(height_map))
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
