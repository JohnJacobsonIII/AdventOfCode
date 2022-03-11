import argparse
import copy


class Scanner:
    def __init__(self, identifier, beacon_list):
        self.identifier = identifier
        self.beacon_list = copy.deepcopy(beacon_list)
        self.relative_distances_per_beacon_list = []
        for beacon1 in beacon_list:
            self.relative_distances_per_beacon_list.append([])
            for beacon2 in beacon_list:
                self.relative_distances_per_beacon_list[-1].append(manhattan_distance(beacon1, beacon2))

    def __str__(self):
        """
        Prints string form of this class.

        Example:
        >>> print(Scanner(0, [[404, -588, -901], [528, -643, 409], [-838, 591, 734]]))
        Scanner 0:
         0: [ 404,-588,-901] [0:    0, 1: 1489, 2: 4056, ]
         1: [ 528,-643, 409] [0: 1489, 1:    0, 2: 2925, ]
         2: [-838, 591, 734] [0: 4056, 1: 2925, 2:    0, ]
        <BLANKLINE>
        """
        assert len(self.beacon_list) == len(self.relative_distances_per_beacon_list)
        returning_string = "Scanner " + str(self.identifier) + ":\n"
        for i in range(len(self.beacon_list)):
            returning_string += str(i).rjust(2) + ": [" + \
                                str(self.beacon_list[i][0]).rjust(4) + "," + \
                                str(self.beacon_list[i][1]).rjust(4) + "," + \
                                str(self.beacon_list[i][2]).rjust(4) + "]"

            returning_string += " ["
            for j in range(len(self.relative_distances_per_beacon_list[i])):
                returning_string += str(j) + ": " + str(self.relative_distances_per_beacon_list[i][j]).rjust(4) + ", "
            returning_string += "]\n"

        return returning_string

    def find_beacon_mapping(self):
        """
        Finds whether the two beacons have overlapping scan cubes and returns the beacon mapping

        Example:
        >>>
        """


def manhattan_distance(coordinates1, coordinates2):
    """
    Returns the Manhattan distance between the two coordinates.

    Example:
    >>> manhattan_distance([404,-588,-901], [528, -643, 409])
    1489
    >>> manhattan_distance([404,-588,-901], [-838, 591, 734])
    4056
    >>> manhattan_distance([528, -643, 409], [-838, 591, 734])
    2925
    """
    return abs(coordinates2[0]-coordinates1[0]) + \
           abs(coordinates2[1]-coordinates1[1]) + \
           abs(coordinates2[2]-coordinates1[2])


def load_data(file_name):
    scanner_list = []
    with open(file_name, 'r') as f:
        for line in f:
            if line == '\n':
                scanner_list.append(Scanner(scanner_identifier, beacons))
                continue
            elif len(line) > 15:
                scanner_identifier = line.strip().split(' ')[2]
                beacons = []
                continue
            else:
                numbers = line.strip().split(',')
                beacons.append([int(numbers[0]), int(numbers[1]), int(numbers[2])])

    scanner_list.append(Scanner(scanner_identifier, beacons))

    return scanner_list


def count_beacons(scanner_list):
    """
    Counts the total number of beacons scanned by all scanners

    """
    beacon_count = 0

    # For beacon1 in scanner1
    #   For beacon2 in scanner2
    for i in range(len(scanner_list)):
        for j in range(len(scanner_list)):
            if i != j:
                found, mapping = scanner_list[i].find_beacon_mapping(scanner_list[j])
                if found:
                    print(mapping)

    #       Compare relative distances of beacon1 and beacon2.
    #       If 12 are the same,
    #           check relative distances of those 12 beacons to see if the same 12 have equal relative distances.
    #           If such two
    return beacon_count


def main():
    parser = argparse.ArgumentParser(description='AoC Day 19 Beacon Scanner')
    parser.add_argument('-f', '--file',
                        help='Input file with coordinates to the target area grid.',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Find maximum y position or 2: Find all initial velocities.',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    scanner_list = load_data(arguments.file)

    for scanner in scanner_list:
        print(scanner)

    if arguments.code == 1:
        print(count_beacons(scanner_list))
    # elif arguments.code == 2:
    #     initial_velocities = all_initial_velocities(target_area)
    #     # print(initial_velocities)
    #     print(len(initial_velocities))
    # else:
    #     print("Selected code not valid")


if __name__ == "__main__":
    main()
