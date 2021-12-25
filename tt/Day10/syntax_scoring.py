import argparse

illegal_char_score_dict = {')': 3, ']': 57, '}': 1197, '>': 25137, '': 0}
autocomplete_char_score_dict = {')': 1, ']': 2, '}': 3, '>': 4}

def load_data(file_name):
    data = []
    with open(file_name, 'r') as f:
        for line in f:
            data.append(line.strip())

    return data


def get_illegal_character(line):
    stack = ''
    for char in line:
        if char == '(' or char == '[' or char == '{' or char == '<':
            stack = stack + char
        elif char == ')' and stack[-1] == '(':
            stack = stack[:-1]
        elif char == ']' and stack[-1] == '[':
            stack = stack[:-1]
        elif char == '}' and stack[-1] == '{':
            stack = stack[:-1]
        elif char == '>' and stack[-1] == '<':
            stack = stack[:-1]
        else:
            return char
    return ''

def return_autocomplete_string(line):
    stack = ''
    for char in line:
        if char == '(' or char == '[' or char == '{' or char == '<':
            stack = stack + char
        elif char == ')' and stack[-1] == '(':
            stack = stack[:-1]
        elif char == ']' and stack[-1] == '[':
            stack = stack[:-1]
        elif char == '}' and stack[-1] == '{':
            stack = stack[:-1]
        elif char == '>' and stack[-1] == '<':
            stack = stack[:-1]
        else:
            return ''
    autocomplete_string = ''
    for i in range(len(stack)-1, -1, -1):
        if stack[i] == '(':
            autocomplete_string = autocomplete_string + ')'
        elif stack[i] == '[':
            autocomplete_string = autocomplete_string + ']'
        elif stack[i] == '{':
            autocomplete_string = autocomplete_string + '}'
        elif stack[i] == '<':
            autocomplete_string = autocomplete_string + '>'
    return autocomplete_string


def calculate_syntax_score(data, code):
    score = 0
    score_list = []

    for line in data:
        if code == 1:
            score = score + illegal_char_score_dict[get_illegal_character(line)]
        elif code == 2:
            score = 0
            for char in return_autocomplete_string(line):
                score = score * 5 + autocomplete_char_score_dict[char]
            if score != 0:
                score_list.append(score)

    if code == 2:
        score_list.sort()
        score = score_list[int(len(score_list)/2)]

    return score



def main():
    parser = argparse.ArgumentParser(description='AoC Day 10 Syntax Scoring')
    parser.add_argument('-f', '--file',
                        help='Input file with each line containing 4 kinds of brackets.',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Calculate syntax error score or 2: Calculate autocompletion score',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    data = load_data(arguments.file)
    # print(data)
    if arguments.code == 1:
        print(calculate_syntax_score(data, 1))
    elif arguments.code == 2:
        print(calculate_syntax_score(data, 2))
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
