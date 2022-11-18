import argparse


class Configuration(object):
    """
    Class representing Configuration

    Attributes
    ----------
    configuration: list
        Configuration of amphipods

    """
    def __init__(self, configuration):
        """
        Class initializer

        Example:
        >>> object1 = load_data("test1.txt")
        >>> assert len(object1.configuration) == 5
        >>> print(len(object1.configuration[-1]))
        13
        >>> print(len(object1.configuration[-2]))
        13
        >>> for line in object1.configuration:
        ...     assert len(line) == 13
        """
        self.configuration = configuration

    def __str__(self):
        """
        String representation of this class

        Example:
        >>> object1 = load_data("test1.txt")
        >>> print(object1) # doctest: +NORMALIZE_WHITESPACE
        Configuration:
        #############
        #...........#
        ###B#C#B#D###
          #A#D#C#A#
          #########
        """
        returning_str = "Configuration:\n"
        for line in self.configuration:
            returning_str += "".join(line) + '\n'

        return returning_str

    def __repr__(self):
        """
        All information about the object of this class

        Example:

        """


class AmphipodStabilization(object):
    """
    Class repre
    """


def load_data(file_name):
    initial_configuration = []
    with open(file_name, 'r') as f:
        for line in f:
            initial_configuration.append(list(line.strip('\n')))

    initial_configuration[-2].append(' ')
    initial_configuration[-2].append(' ')
    initial_configuration[-1].append(' ')
    initial_configuration[-1].append(' ')

    return Configuration(initial_configuration)


def main():
    parser = argparse.ArgumentParser(description='AoC Day 23 Amphipod')
    parser.add_argument('-f', "--file",
                        help="Input file containing initial configuration of amphipods.",
                        default="input.txt")
    parser.add_argument('-c', "--code",
                        help="Select 1: Find least energy to get to final amphipod configuration, 2: ",
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    initial_configuration = load_data(arguments.file)

    print(initial_configuration)

    if arguments.code == 1:
        print()
    # elif arguments.code == 2:
    #     print(reactor_reboot_object.reboot_reactor(2))
    # else:
    #     print("Selected code not valid")


if __name__ == "__main__":
    main()
