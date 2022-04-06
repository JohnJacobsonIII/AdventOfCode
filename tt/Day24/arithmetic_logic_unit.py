import argparse


class ArithmeticLogicUnit(object):
    """
    Class representing amphipod configuration

    Attributes
    ----------
    """
    def __init__(self):
        """
        Class initializer

        Example:
        """
        self.space_list = []

    def __str__(self):
        """
        String representation of this class

        Example:
        """
        returning_str = "Cubes:\n"
        for space in self.space_list:
            for coordinate, interval in space.items():
                returning_str += coordinate + ": [" + str(interval[0]).ljust(6) + ',' + str(interval[1]).ljust(6) + "],"

        return returning_str


def load_data(file_name):
    reboot_steps = []
    with open(file_name, 'r') as f:
        for line in f:
            instruction, cuboid = line.strip().split(' ')
            cuboid = cuboid.split(',')
            cuboid_coords = {coordinate[0]: [int(coordinate[2:].split('..')[0]), int(coordinate[2:].split('..')[1])] for
                             coordinate in cuboid}
            reboot_steps.append((True if instruction=="on" else False, cuboid_coords))

    return


def main():
    parser = argparse.ArgumentParser(description='AoC Day 24 Arithmetic Logic Unit')
    parser.add_argument('-f', "--file",
                        help="Input file with each line containing a program instruction.",
                        default="input.txt")
    parser.add_argument('-c', "--code",
                        help="Select 1: Find the largest model number that is valid, 2: ",
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    temp = load_data(arguments.file)

    # print(reactor_reboot_object)

    if arguments.code == 1:
        print()
    # elif arguments.code == 2:
    #     print(reactor_reboot_object.reboot_reactor(2))
    # else:
    #     print("Selected code not valid")


if __name__ == "__main__":
    main()