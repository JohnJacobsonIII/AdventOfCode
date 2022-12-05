import argparse

amphipod_move_cost={'A':1,
                    'B':10,
                    'C':100,
                    'D':1000}

class Configuration(object):
    """
    Class representing Configuration

    Attributes
    ----------
    configuration: list
        Configuration of amphipods
    amphipod_positions: dict
        Dictionary (string->list(list(int))) mapping amphipod type and list of coordinates on the grid.

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
        >>> assert len(object1.amphipod_positions['A']) == len(object1.amphipod_positions['B']) == len(object1.amphipod_positions['C']) == len(object1.amphipod_positions['D'])
        """
        self.configuration = configuration
        self.amphipod_positions = {'A': [], 'B': [], 'C': [], 'D': []}
        for i in range(len(self.configuration)):
            for j in range(len(self.configuration[i])):
                if self.configuration[i][j].isalpha():
                    assert self.configuration[i][j] in self.amphipod_positions
                    self.amphipod_positions[self.configuration[i][j]].append([i, j])

        assert len(self.amphipod_positions['A']) == \
               len(self.amphipod_positions['B']) == \
               len(self.amphipod_positions['C']) == \
               len(self.amphipod_positions['D'])

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

    def move_amphipod(self, source, destination):
        """
        Moves the amphipod at source to destination.

        Parameters
        ----------
        source : tuple
            Coordinates in grid of source having an amphipod.
        destination : tuple
            Coordinates in grid of empty destination.

        Returns
        -------
        (string, int)
            Returns the amphipod type and number of steps the amphipod moved.

        Example:
        
        """
        # Ensuring we have coordinates and not some garbage
        assert len(source) == len(destination) == 2

        # Amphipods cannot move from hall to hall.
        assert not (source[0] == 1 and destination[0]==1)
        assert self.configuration[source[0], source[1]].isalpha() and self.configuration[destination[0], destination[1]] == '.'
        assert destination[0] == 1 and destination[1] not in [3, 5, 6, 9]

        # Initializing number of steps to 0
        steps = 0
        # Amphipod is in hall
        if source[0] == 1:
            # Move along hall to outside destination room
            if source[1] < destination[1]:
                for i in range(source[1], destination[1]+1):
                    if self.configuration[1][i] != '.':
                        print("Somethings blocking path")
                        print(self)
                        return '', 0
                steps += destination[1] - source[1]
            else:
                for i in range(source[1], destination[1]-1, -1):
                    if self.configuration[1][i] != '.':
                        print("Somethings blocking path")
                        print(self)
                        return '', 0
                steps += source[1] - destination[1]

            # Move into room
            for i in range(2, destination[0]+1):
                if self.configuration[i][destination[1]] != '.':
                    print("Somethings blocking path")
                    print(self)
                    return '', 0
            steps += destination[0]-1
        # Amphipod is in room
        else:
            # Moving out of room
            for i in range(source[0], 0, -1):
                if self.configuration[i][source[1]] != '.':
                    print("Somethings blocking path")
                    print(self)
                    return '', 0
            steps += source[1]-1

            # Move along hall
            if source[1] < destination[1]:
                for i in range(source[1], destination[1] + 1):
                    if self.configuration[1][i] != '.':
                        print("Somethings blocking path")
                        print(self)
                        return '', 0
                steps += destination[1] - source[1]
            else:
                for i in range(source[1], destination[1] - 1, -1):
                    if self.configuration[1][i] != '.':
                        print("Somethings blocking path")
                        print(self)
                        return '', 0
                steps += source[1] - destination[1]

            if destination[0] != 1:
                # Move into room
                for i in range(2, destination[0] + 1):
                    if self.configuration[i][destination[1]] != '.':
                        print("Somethings blocking path")
                        print(self)
                        return '', 0
                steps += destination[0] - 1

        return self.configuration[source[0], source[1]], steps


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
