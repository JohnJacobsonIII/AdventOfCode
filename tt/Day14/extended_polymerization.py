import argparse


def load_data(file_name):
    rules = {}
    chars = set()
    with open(file_name, 'r') as f:
        polymer = f.readline().strip()
        for char in polymer:
            chars.add(char)
        f.readline()
        for line in f:
            rules[line.strip().split(' -> ')[0]] = line.strip().split(' -> ')[1]
            for char in line.strip().split(' -> ')[0]:
                chars.add(char)
            for char in line.strip().split(' -> ')[1]:
                chars.add(char)

    return polymer, rules, dict.fromkeys(chars, 0)


def next_step(rules, rule_counts, char_counts):
    new_rule_counts = dict.fromkeys(rules.keys(), 0)

    for letter_pair, count in rule_counts.items():
        new_rule_counts[letter_pair[0] + rules[letter_pair[0] + letter_pair[1]]] = \
            new_rule_counts[letter_pair[0] + rules[letter_pair]] + rule_counts[letter_pair]
        new_rule_counts[rules[letter_pair] + letter_pair[1]] = \
            new_rule_counts[rules[letter_pair] + letter_pair[1]] + rule_counts[letter_pair]
        char_counts[rules[letter_pair]] = char_counts[rules[letter_pair]] + count

    return new_rule_counts


def largest_difference(polymer, rules, char_counts, n):
    new_rule_counts = dict.fromkeys(rules.keys(), 0)

    for i in range(len(polymer) - 1):
        new_rule_counts[polymer[i] + polymer[i + 1]] = new_rule_counts[polymer[i] + polymer[i + 1]] + 1
        char_counts[polymer[i]] = char_counts[polymer[i]] + 1
    char_counts[polymer[len(polymer) - 1]] = char_counts[polymer[len(polymer) - 1]] + 1

    for step in range(n):
        new_rule_counts = next_step(rules, new_rule_counts, char_counts)

    # count_chars(new_rule_counts, rules, char_counts, polymer[0], polymer[1])
    return max(char_counts.values()) - min(char_counts.values())


def main():
    parser = argparse.ArgumentParser(description='AoC Day 14 Extended Polymerization')
    parser.add_argument('-f', '--file',
                        help='Input file with each line representing .',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Subtract most and least common elements after 10 and 40 steps.',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    polymer, rules, char_counts = load_data(arguments.file)
    if arguments.code == 1:
        print(largest_difference(polymer, rules, char_counts, 40))
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
