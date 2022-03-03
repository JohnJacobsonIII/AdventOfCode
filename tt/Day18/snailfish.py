import argparse
import itertools
import math
from colorama import Fore, Style
from copy import deepcopy

packet_metadata = []


class SnailFishNumber:
    def __init__(self, left, right):
        self.type = -1
        self.parent = None
        if isinstance(left, SnailFishNumber):
            left.type = 0
            left.parent = self
        if isinstance(right, SnailFishNumber):
            right.type = 1
            right.parent = self
        self.left = left
        self.left_depth = max(self.left.left_depth, self.left.right_depth)+1 if isinstance(left, SnailFishNumber) else 0
        self.right = right
        self.right_depth = max(self.right.left_depth, self.right.right_depth)+1 if isinstance(right, SnailFishNumber) else 0

    def __str__(self):
        return '[' + str(self.left) + ',' + str(self.right) + ']'

    def __add__(self, other):
        """
        Adds two SnailFishNumber

        Example:
        >>> print(parse_snailfish_number('[[[4,3],4],4],[7,[[8,4],9]]]')[0] + parse_snailfish_number('1,1]')[0])
        [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
        >>> print(parse_snailfish_number('1,1]')[0] + parse_snailfish_number('2,2]')[0])
        [[1,1],[2,2]]
        >>> print(parse_snailfish_number('[[1,1],[2,2]],[3,3]]')[0] + parse_snailfish_number('4,4]')[0])
        [[[[1,1],[2,2]],[3,3]],[4,4]]
        >>> print(parse_snailfish_number('[[[1,1],[2,2]],[3,3]],[4,4]]')[0] + parse_snailfish_number('5,5]')[0])
        [[[[3,0],[5,3]],[4,4]],[5,5]]
        >>> print(parse_snailfish_number('[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]')[0] + parse_snailfish_number('7,[5,[[3,8],[1,4]]]]')[0])
        [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]
        """
        new_snailfish_number = SnailFishNumber(self, other)
        new_snailfish_number.recalibrate_depths()

        loop = True
        # Repeat till no condition is satisfied
        step = 0
        while loop:
            # print(new_snailfish_number)
            step += 1
            if new_snailfish_number.left_depth==4 or new_snailfish_number.right_depth==4:
                # print(str(step).ljust(4), 'Exploding: ', end='')
                # print(new_snailfish_number.print_with_exploding_highlight(5, False)[0])
                new_snailfish_number.explode(5)
                new_snailfish_number.recalibrate_depths()
                # Loop again to check the first rule
                continue
            # print(str(step).ljust(4), 'Splitting: ', end='')
            # print(new_snailfish_number.print_with_splitting_highlight(False)[0])
            if new_snailfish_number.split():
                new_snailfish_number.recalibrate_depths()
            else:
                # Time to exit loop
                loop = False
        return new_snailfish_number

    def __eq__(self, other):
        """
        Compares if two SnailFishNumbers are the same

        Example:
        >>> example1 = parse_snailfish_number('[[[0,7],4],[9,[0,9]]],[1,1]]')[0]
        >>> example2 = parse_snailfish_number('[[[0,7],4],[9,[0,9]]],[1,1]]')[0]
        >>> example1 == example2
        True
        """
        if isinstance(other, SnailFishNumber):
            return self.type == other.type and \
                   self.parent == other.parent and \
                   self.left_depth == other.left_depth and \
                   self.right_depth == other.right_depth
        return False

    def print_with_exploding_highlight(self, assumed_depth, number_found):
        """
        Highlights the exploding pair

        Example:
        >>> print(parse_snailfish_number('[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]')[0].print_with_exploding_highlight(5, False)[0])
        [[[[\x1b[32m[4,3]\x1b[0m,4],4],[7,[[8,4],9]]],[1,1]]

        """
        returning_string = ''

        # Found pair to be exploded
        if assumed_depth == 1 and not number_found:
            returning_string += Fore.GREEN + '[' + str(self.left) + ',' + str(self.right) + ']' + Style.RESET_ALL
            number_found = True
        else:
            returning_string += '['

            # Probing the left branch
            if isinstance(self.left, int):
                returning_string += str(self.left)
            else:
                left_string, number_found = self.left.print_with_exploding_highlight(assumed_depth-1, number_found)
                returning_string += left_string
            returning_string += ','

            # Probing the right branch
            if isinstance(self.right, int):
                returning_string += str(self.right)
            else:
                right_string, number_found = self.right.print_with_exploding_highlight(assumed_depth-1, number_found)
                returning_string += right_string

            returning_string += ']'
        return returning_string, number_found

    def print_with_splitting_highlight(self, number_found):
        """
        Highlights the splitting number

        Example:
        >>> example1 = parse_snailfish_number('[[[0,7],4],[9,[0,9]]],[1,1]]')[0]
        >>> example1.left.right.left += 6
        >>> example1.left.right.right.right += 4
        >>> print(example1.print_with_splitting_highlight(False)[0])
        [[[[0,7],4],[\x1b[32m15\x1b[0m,[0,13]]],[1,1]]
        """
        returning_string = '['

        # Probing the left branch
        if isinstance(self.left, int):
            # Found number to be split
            if self.left > 9 and not number_found:
                returning_string += Fore.GREEN + str(self.left) + Style.RESET_ALL
                number_found = True
            else:
                returning_string += str(self.left)
        else:
            left_string, number_found = self.left.print_with_splitting_highlight(number_found)
            returning_string += left_string

        returning_string += ','

        # Probing the right branch
        if isinstance(self.right, int):
            # Found number to be split
            if self.right > 9 and not number_found:
                returning_string += Fore.GREEN + str(self.right) + Style.RESET_ALL
                number_found = True
            else:
                returning_string += str(self.right)
        else:
            right_string, number_found = self.right.print_with_splitting_highlight(number_found)
            returning_string += right_string
        returning_string += ']'

        return returning_string, number_found

    def split(self):
        """
        Checks whether all elements in tree are less than 10

        Example:
        >>> example1 = parse_snailfish_number('[[[0,7],4],[9,[0,9]]],[1,1]]')[0]
        >>> example1.left.right.left += 6
        >>> example1.left.right.right.right += 4
        >>> example1.split()
        True
        >>> print(example1)
        [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
        >>> example2 = parse_snailfish_number('[[[0,7],4],[[7,8],[0,9]]],[1,1]]')[0]
        >>> example2.left.right.right.right += 4
        >>> example2.split()
        True
        >>> print(example2)
        [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]
        >>> example3 = parse_snailfish_number('[1,9],[8,5]]')[0]
        >>> example3.split()
        False
        >>> print(example3)
        [[1,9],[8,5]]
        >>> example4 = parse_snailfish_number('[[[7,7],[7,8]],[[9,5],[8,0]]],[[[9,9],9],[8,[9,0]]]]')[0]
        >>> example4.right.left.left.right += 1
        >>> example4.right.left.right += 11
        >>> example4.split()
        True
        >>> print(example4)
        [[[[7,7],[7,8]],[[9,5],[8,0]]],[[[9,[5,5]],20],[8,[9,0]]]]
        """
        result = False
        if not isinstance(self.left, SnailFishNumber) and self.left > 9:
            new_pair = SnailFishNumber(math.floor(self.left / 2), math.ceil(self.left / 2))
            self.left = new_pair
            self.left.parent = self
            self.left.type = 0
            result = True
        elif isinstance(self.left, SnailFishNumber):
            result = self.left.split()

        if not isinstance(self.right, SnailFishNumber) and self.right > 9 and not result:
            new_pair = SnailFishNumber(math.floor(self.right / 2), math.ceil(self.right / 2))
            self.right = new_pair
            self.right.parent = self
            self.right.type = 1
            result = True
        elif isinstance(self.right, SnailFishNumber) and not result:
            result = self.right.split()

        return result


    def explode(self, assumed_branch_depth):
        """
        Find a pair to explode, explodes it and recalibrates depths
        To explode, the pair of numbers to be exploded is added to the respective elements and sets the pair to 0

        Example:
        >>> example1 = parse_snailfish_number('[[[[9,8],1],2],3],4]')[0]
        >>> example1.explode(5)
        (9, 0)
        >>> print(example1)
        [[[[0,9],2],3],4]
        >>> example2 = parse_snailfish_number('7,[6,[5,[4,[3,2]]]]]')[0]
        >>> example2.explode(5)
        (2, 1)
        >>> print(example2)
        [7,[6,[5,[7,0]]]]
        >>> example3 = parse_snailfish_number('[6,[5,[4,[3,2]]]],1]')[0]
        >>> example3.explode(5)
        (2, -1)
        >>> print(example3)
        [[6,[5,[7,0]]],3]
        >>> example4 = parse_snailfish_number('[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]')[0]
        >>> example4.explode(5)
        (3, -1)
        >>> print(example4)
        [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
        """
        if assumed_branch_depth == 1:
            assert isinstance(self.left, int)
            assert isinstance(self.right, int)
            if not self.type:
                self.parent.left = 0
                if isinstance(self.parent.right, SnailFishNumber) and isinstance(self.parent.right.left, int):
                    self.parent.right.left += self.right
                elif isinstance(self.parent.right, int):
                    self.parent.right += self.right
                return self.left, 0
            elif self.type:
                self.parent.right = 0
                if isinstance(self.parent.left, SnailFishNumber) and isinstance(self.parent.left.right, int):
                    self.parent.left.right += self.left
                elif isinstance(self.parent.left, int):
                    self.parent.left += self.left
                return self.right, 1
            else:
                print("Some problem here, please check.")
            print("Should never land here. Check why Redo code")
            exit(1)

        if self.left_depth == assumed_branch_depth-1:
            element, index = self.left.explode(assumed_branch_depth-1)
        elif self.right_depth == assumed_branch_depth-1:
            element, index = self.right.explode(assumed_branch_depth-1)
        else:
            print("Error in depth calibration. Check.")

        if index == -1:
            return element, -1
        elif not self.type and not index:
            return element, index
        elif self.type and index:
            return element, index
        elif self.parent:
            if self.type:
                if isinstance(self.parent.left, SnailFishNumber):
                    self.parent.left.add_number_in_postorder(element, index)
                elif isinstance(self.parent.left, int):
                    self.parent.left += element
            elif not self.type:
                if isinstance(self.parent.right, SnailFishNumber):
                    self.parent.right.add_number_in_postorder(element, index)
                elif isinstance(self.parent.right, int):
                    self.parent.right += element
            return element, -1
        elif not self.parent:
            if not index:
                # print("No element to the left")
                return element, index
            elif index:
                # print("No element to the right")
                return element, index

        print("Program should never reach here. Check")


    def add_number_in_postorder(self, element, order):
        """
        Adds element to the last or first element in postorder.
        order = 0 -> Add to last element.
        order = 1 -> Add to first element.

        Example:
        >>> example1 = parse_snailfish_number('[[[0,1],2],3],4]')[0]
        >>> example1.left.left.left.add_number_in_postorder(8,0)
        >>> print(example1)
        [[[[0,9],2],3],4]
        >>> example2 = parse_snailfish_number('7,[6,[5,[4,0]]]]')[0]
        >>> example2.right.right.right.add_number_in_postorder(3,1)
        >>> print(example2)
        [7,[6,[5,[7,0]]]]
        >>> example3 = parse_snailfish_number('[6,[5,[4,0]]],1]')[0]
        >>> example3.left.right.right.add_number_in_postorder(3,1)
        >>> example3.add_number_in_postorder(2,0)
        >>> print(example3)
        [[6,[5,[7,0]]],3]
        >>> example4 = parse_snailfish_number('[3,[2,[8,0]]],[6,[5,[4,[3,2]]]]]')[0]
        >>> example4.right.add_number_in_postorder(3,1)
        >>> print(example4)
        [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
        """
        # Add to the last element in postorder
        if not order:
            if isinstance(self.right, int):
                self.right += element
            elif isinstance(self.right, SnailFishNumber):
                self.right.add_number_in_postorder(element, order)
        # Add to the first element in postorder
        elif order:
            if isinstance(self.left, int):
                self.left += element
            elif isinstance(self.left, SnailFishNumber):
                self.left.add_number_in_postorder(element, order)


    def recalibrate_depths(self):
        """
        Resets the depths of all the children

        Example:
        >>> parse_snailfish_number('1,2]')[0].recalibrate_depths()
        1
        >>> parse_snailfish_number('[1,2],[[3,4],5]]')[0].recalibrate_depths()
        3
        >>> parse_snailfish_number('[[[3,4],5],[1,2]],[1,1]]')[0].recalibrate_depths()
        4
        """
        self.left_depth = self.left.recalibrate_depths() if isinstance(self.left, SnailFishNumber) else 0
        self.right_depth = self.right.recalibrate_depths() if isinstance(self.right, SnailFishNumber) else 0

        return max(self.left_depth, self.right_depth)+1

    def check_less_than_10(self):
        """
        Checks whether all elements in tree are less than 10

        Example:
        >>> parse_snailfish_number('1,2]')[0].check_less_than_10()
        True
        >>> parse_snailfish_number('[1,2],[[3,4],5]]')[0].check_less_than_10()
        True
        >>> parse_snailfish_number('[[[3,4],5],[1,2]],[1,1]]')[0].check_less_than_10()
        True
        >>> temp = parse_snailfish_number('1,2]')[0]
        >>> temp.left+=9
        >>> temp.check_less_than_10()
        False
        """
        if not isinstance(self.left, SnailFishNumber):
            result = self.left<10
        else:
            result = self.left.check_less_than_10()
        if not isinstance(self.right, SnailFishNumber):
            return result and self.right<10
        else:
            return result and self.right.check_less_than_10()

    def magnitude(self):
        """
        Calculates the magnitude of the snailfish number.
        Formula is recursive 3*magnitude of left element + 2*magnitude of right element
        magnitude of a regular number is the number itself

        Example:
        >>> parse_snailfish_number('[1,2],[[3,4],5]]')[0].magnitude()
        143
        >>> parse_snailfish_number('[[[0,7],4],[[7,8],[6,0]]],[8,1]]')[0].magnitude()
        1384
        >>> parse_snailfish_number('[[[1,1],[2,2]],[3,3]],[4,4]]')[0].magnitude()
        445
        >>> parse_snailfish_number('[[[3,0],[5,3]],[4,4]],[5,5]]')[0].magnitude()
        791
        >>> parse_snailfish_number('[[[5,0],[7,4]],[5,5]],[6,6]]')[0].magnitude()
        1137
        >>> parse_snailfish_number('[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]')[0].magnitude()
        3488
        >>> parse_snailfish_number('[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]')[0].magnitude()
        4140
        """
        left_magnitude = 3*self.left.magnitude() if isinstance(self.left, SnailFishNumber) else 3*self.left
        right_magnitude = 2*self.right.magnitude() if isinstance(self.right, SnailFishNumber) else 2*self.right

        return left_magnitude+right_magnitude


def parse_snailfish_number(line):
    """
    Parses snailfish numbers

    Example:
    >>> print(parse_snailfish_number('1,2]')[0])
    [1,2]
    >>> print(parse_snailfish_number('[1,2],3]')[0])
    [[1,2],3]
    >>> print(parse_snailfish_number('9,[8,7]]')[0])
    [9,[8,7]]
    >>> print(parse_snailfish_number('[[[1,2],[3,4]],[[5,6],[7,8]]],9]')[0])
    [[[[1,2],[3,4]],[[5,6],[7,8]]],9]
    >>> print(parse_snailfish_number('[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]')[0])
    [[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]
    """
    snail_fish_data = []

    # Looping twice because there are two snailfish numbers within a snailfish number.
    for i in range(2):
        if line[0] != '[':
            snail_fish_data.append(int(line[0]))
            line = line[2:]
        else:
            snailfish_number, line = parse_snailfish_number(line[1:])
            snail_fish_data.append(snailfish_number)

    return SnailFishNumber(snail_fish_data[0], snail_fish_data[1]), line[1:]


def load_data(file_name):
    list_snailfish_numbers = []
    with open(file_name, 'r') as f:
        for line in f:
            line=line.strip()
            snailfish_number, remaining_string = parse_snailfish_number(line[1:])
            assert remaining_string == ''
            list_snailfish_numbers.append(snailfish_number)
            # print(snailfish_number)

    return list_snailfish_numbers


# def magnitude(snailfish_string):
#     """
#     Gives the magnitude of the string snailfish number
#
#     Example:
#     >>> magnitude('9,1]')[0]
#     29
#     >>> magnitude('1,9]')[0]
#     21
#     >>> magnitude('[9,1],[1,9]]')[0]
#     129
#     >>> magnitude('[1,2],[[3,4],5]]')[0]
#     143
#     >>> magnitude('[[[0,7],4],[[7,8],[6,0]]],[8,1]]')[0]
#     1384
#     >>> magnitude('[[[1,1],[2,2]],[3,3]],[4,4]]')[0]
#     445
#     >>> magnitude('[[[3,0],[5,3]],[4,4]],[5,5]]')[0]
#     791
#     >>> magnitude('[[[5,0],[7,4]],[5,5]],[6,6]]')[0]
#     1137
#     >>> magnitude('[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]')[0]
#     3488
#     """
#     magnitudes=[]
#
#     # Looping twice because there are two snailfish numbers within a snailfish number.
#     for i in range(2):
#         if snailfish_string[0] != '[':
#             magnitudes.append(int(snailfish_string[0]))
#             snailfish_string = snailfish_string[2:]
#         else:
#             magnitude_of_snailfish_number, snailfish_string = magnitude(snailfish_string[1:])
#             magnitudes.append(magnitude_of_snailfish_number)
#
#     return 3*magnitudes[0] + 2*magnitudes[1], snailfish_string[1:]
#
#
# def explode(snailfish_string, current_index, count_open_brackets):
#     """
#     Explodes the leftmost pair nested inside 4 snailfish numbers
#
#     Example:
#     >>> explode('[[[[9,8],1],2],3],4]')
#     [[[[0,9],2],3],4]
#     >>> explode('7,[6,[5,[4,[3,2]]]]]')
#     [7,[6,[5,[7,0]]]]
#     >>> explode('[6,[5,[4,[3,2]]]],1]')
#     [[6,[5,[7,0]]],3]
#     >>> explode('[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]')
#     [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
#     >>> explode('[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]')
#     [[3,[2,[8,0]]],[9,[5,[7,0]]]]
#     """
#     magnitudes = []
#
#     # This ensures we reached the pair to be exploded
#     if count_open_brackets == 5:
#         # Asserting that we have a number
#         assert snailfish_string[current_index].isdigit()
#
#         # Looping twice since there are two numbers in a pair to be exploded
#         for i in range(2):
#             # Forming the number from multiple digits
#             local_index = current_index
#             number = 0
#             multiplier = 1
#             while snailfish_string[local_index].isdigit():
#                 number += multiplier*snailfish_string[local_index]
#                 multiplier *= 10
#                 local_index += 1
#
#             # Add number to the immediate left regular number
#             if i == 0:
#                 local_index_to_add_to_left = current_index-1
#                 while not snailfish_string[local_index_to_add_to_left].isdigit():
#                     local_index_to_add_to_left -= 1
#
#                 # Asserting we reached the end of the first number in pair and that it was indeed the first number since
#                 # it was followed by a comma.
#                 assert snailfish_string[local_index] == ','
#                 local_index += 1
#             # Add number to the immediate right regular number
#             elif i == 1:
#
#
#
#             if snailfish_string[current_index] ==
#
#     # Looping twice because there are two snailfish numbers within a snailfish number.
#     for i in range(2):
#         if snailfish_string[0] != '[':
#             magnitudes.append(int(snailfish_string[0]))
#             snailfish_string = snailfish_string[2:]
#         else:
#             magnitude_of_snailfish_number, snailfish_string = magnitude(snailfish_string[1:])
#             magnitudes.append(magnitude_of_snailfish_number)
#
#     return 3 * magnitudes[0] + 2 * magnitudes[1], snailfish_string[1:]


def main():
    parser = argparse.ArgumentParser(description='AoC Day 18 SnailFish')
    parser.add_argument('-f', '--file',
                        help='Input file with nested list of pairs of integers.',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Sum of Snailfish numbers or 2: Find greatest magnitude of two snailfish '
                             'numbers',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    # magnitude()
    # exit(0)
    list_snailfish_numbers = load_data(arguments.file)
    if arguments.code == 1:
        result = list_snailfish_numbers[0]
        for num in list_snailfish_numbers[1:]:
            result = result + num

        print(result.magnitude())
    elif arguments.code == 2:
        greatest_magnitude = 0
        for i in range(len(list_snailfish_numbers)):
            for j in range(i+1, len(list_snailfish_numbers)):
                for k in range(2):
                    snailfish_number_one = deepcopy(list_snailfish_numbers[i])
                    snailfish_number_two = deepcopy(list_snailfish_numbers[j])

                    if not k:
                        result_magnitude = (snailfish_number_one + snailfish_number_two).magnitude()
                    elif k:
                        result_magnitude = (snailfish_number_two + snailfish_number_one).magnitude()
                    if result_magnitude > greatest_magnitude:
                        greatest_magnitude = result_magnitude

        print(greatest_magnitude)
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
