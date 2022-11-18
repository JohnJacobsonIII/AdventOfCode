import argparse
import copy
from collections import defaultdict

edges = defaultdict(set)


def load_data(file_name):
    with open(file_name, 'r') as f:
        for line in f:
            vertices = line.strip().split('-')
            edges[vertices[0]].add(vertices[1])
            edges[vertices[1]].add(vertices[0])

    return


def total_paths():
    completed_paths = []
    paths = []
    paths.append(['start'])

    while paths:
        current_path = paths.pop(0)
        for next_cave in edges[current_path[-1]]:
            # print(next_cave)
            new_path = copy.deepcopy(current_path)
            new_path.append(next_cave)

            if next_cave.islower() and next_cave in current_path:
                continue

            if next_cave == 'end':
                completed_paths.append(new_path)
            else:
                paths.append(new_path)
    # print(completed_paths)
    return len(completed_paths)


def total_paths_with_one_duplicate():
    completed_paths = []
    paths = []
    secondary_paths = []
    paths.append(['start'])

    while paths:
        current_path = paths.pop(0)
        for next_cave in edges[current_path[-1]]:
            # print(next_cave)
            new_path = copy.deepcopy(current_path)
            new_path.append(next_cave)

            if next_cave.islower() and next_cave in current_path:
                if next_cave != "start" and next_cave != "end":
                    new_path.append(next_cave)
                    secondary_paths.append(new_path)
                continue

            if next_cave == 'end':
                completed_paths.append(new_path)
            else:
                paths.append(new_path)

    while secondary_paths:
        current_path = secondary_paths.pop(0)
        for next_cave in edges[current_path[-1]]:
            # print(next_cave)
            new_path = copy.deepcopy(current_path)
            new_path.append(next_cave)

            if next_cave.islower() and next_cave in current_path:
                continue

            if next_cave == 'end':
                completed_paths.append(new_path)
            else:
                secondary_paths.append(new_path)

    return len(completed_paths)


def main():
    parser = argparse.ArgumentParser(description='AoC Day 12 Passage Pathing')
    parser.add_argument('-f', '--file',
                        help='Input file with each line representing a path between two caves.',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Find total number of paths or 2: Find total paths with one small cave repeated.',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    load_data(arguments.file)
    # print(edges)
    if arguments.code == 1:
        print(total_paths())
    elif arguments.code == 2:
        print(total_paths_with_one_duplicate())
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
