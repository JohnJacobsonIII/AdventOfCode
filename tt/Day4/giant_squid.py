import argparse


def load_data(file_name):
    boards = []
    with open(file_name, 'r') as f:
        bingo_numbers = [int(n) for n in f.readline().strip().split(',')]
        # print(bingo_numbers)
        for line in f:
            if line == '\n':
                boards.append([])
                continue
            boards[len(boards) - 1].append([int(n) for n in line.strip().split()])

        # print(data)
    return bingo_numbers, boards


def is_bingo_achieved(board):
    for row in board:
        if all(elem == -1 for elem in row):
            return True

    for column in range(len(board)):
        if all(elem == -1 for elem in [row[column] for row in board]):
            return True

    return False


def mark_number(number, boards):
    index = []
    for k in range(len(boards)):
        for i in range(len(boards[k])):
            if number in boards[k][i]:
                for j in range(len(boards[k][i])):
                    if number == boards[k][i][j]:
                        boards[k][i][j] = -1

        if is_bingo_achieved(boards[k]):
            index.append(k)

    if index:
        return True, index
    return False, index


def calculate_score(board, number):
    score = 0

    for row in board:
        for i in range(len(row)):
            if row[i] != -1:
                score += row[i]

    return score * number


def first_board(bingo_numbers, boards):
    for number in bingo_numbers:
        (done, index) = mark_number(number, boards)
        if done:
            return calculate_score(boards[index[0]], number)

    return 0


def last_board(bingo_numbers, boards):
    for number in bingo_numbers:
        (done, index) = mark_number(number, boards)
        if done:
            for i in range(len(index)):
                if len(boards) == 1 or number == bingo_numbers[len(bingo_numbers) - 1]:
                    return calculate_score(boards[0], number)
                else:
                    boards.pop(index[i] - i)

    return 0


def main():
    parser = argparse.ArgumentParser(description='AoC Day 1 Sonar Sweeper')
    parser.add_argument('-f', '--file',
                        help='Input file containing integer values on each line representing depths',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Select 1st winning board or 2: Select last winning board',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    (bingo_numbers, boards) = load_data(arguments.file)
    if arguments.code == 1:
        print(first_board(bingo_numbers, boards))
    elif arguments.code == 2:
        print(last_board(bingo_numbers, boards))
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
