import argparse


def load_data(file_name):
    coordinates = set()
    fold_instructions = []
    with open(file_name, 'r') as f:
        for line in f:
            if line == '\n':
                break
            coord = line.strip().split(',')
            coordinates.add((int(coord[0]), int(coord[1])))
        for line in f:
            fold_inst = line.strip().split()[2].split('=')
            fold_instructions.append((fold_inst[0], int(fold_inst[1])))

    return coordinates, fold_instructions


def fold_paper(coordinates, fold_instruction):
    new_coordinates = set()
    (fold_along, line_coordinate) = fold_instruction

    if fold_along == 'x':
        for coordinate in coordinates:
            x = coordinate[0]
            y = coordinate[1]
            if x > line_coordinate:
                new_coordinates.add((x - 2 * (x - line_coordinate), y))
            else:
                new_coordinates.add((x, y))
    elif fold_along == 'y':
        for coordinate in coordinates:
            x = coordinate[0]
            y = coordinate[1]
            if y > line_coordinate:
                new_coordinates.add((x, y - 2 * (y - line_coordinate)))
            else:
                new_coordinates.add((x, y))

    return new_coordinates

def print_grid(coordinates, max_x, max_y):
    grid = []
    for i in range(max_y):
        grid.append([])
        for j in range(max_x):
            grid[i].append('.')

    for coordinate in coordinates:
        grid[coordinate[0]][coordinate[1]] = '#'

    for i in range(max_y):
        print(grid[i])

    return

def dots_after_first_fold(coordinates, fold_instructions):
    new_coordinates = fold_paper(coordinates, fold_instructions[0])

    return len(new_coordinates)


def dots_after_all_folds(coordinates, fold_instructions):
    new_coordinates = coordinates

    max_x = max([coordinate[0] for coordinate in coordinates])
    max_y = max([coordinate[1] for coordinate in coordinates])

    for fold_instruction in fold_instructions:
        new_coordinates = fold_paper(new_coordinates, fold_instruction)

    print_grid(new_coordinates, max_x, max_y)

    return


def main():
    parser = argparse.ArgumentParser(description='AoC Day 13 Transparent Origami')
    parser.add_argument('-f', '--file',
                        help='Input file with each line representing coordinates of dots. The coordinates are followed '
                             'by lines along which to fold the paper.',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Dots after first fold or 2: Code after all folds.',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    coordinates, fold_instructions = load_data(arguments.file)
    # print(coordinates)
    # print(fold_instructions)
    if arguments.code == 1:
        print(dots_after_first_fold(coordinates, fold_instructions))
    elif arguments.code == 2:
        dots_after_all_folds(coordinates, fold_instructions)
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
