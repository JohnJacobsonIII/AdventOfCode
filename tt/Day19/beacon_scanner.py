import argparse
import copy


class SpaceScanner:
    """
    Attributes
    ----------
    scanner_list : list
        Contains the beacon scans of all scanners
    mapping_dict : dict
        (scanner_one, scanner_two) -> {dict[beacon_in_scanner_one -> beacon_in_scanner_two]} mapping of beacons.
    orientation_dict : dict
        (scanner_one, scanner_two) -> ['x', 'y', 'z']. The permutation of coordinates required to orient scanner_two in
        the same direction and orientation as scanner_one.
    translation_dict : dict
        (scanner_one, scanner_two) -> [d_x, d_y, d_z]. The translation of coordinates required for beacons in
        scanner_two after shifting the origin from scanner_two to scanner_one.
    """
    def __init__(self, scanner_list):
        self.scanner_list = scanner_list
        self.mapping_dict = {}
        self.orientation_dict = {}
        self.translation_dict = {}
        self.beacons_wrt_first_scanner = set()

    def __str__(self):
        """
        Example:
        >>> print(
        ... Scanner(0, [[404, -588, -901], [528, -643, 409], [-838, 591, 734]]),
        ... Scanner(1, [[686, 422, 578], [578, 423, 415], [515, 917, -361]]))
        Scanner 0:
         0: [ 404,-588,-901] [0:    0, 1: 1489, 2: 4056, ]
         1: [ 528,-643, 409] [0: 1489, 1:    0, 2: 2925, ]
         2: [-838, 591, 734] [0: 4056, 1: 2925, 2:    0, ]
         Scanner 1:
         0: [ 686, 422, 578] [0:    0, 1:  272, 2: 1605, ]
         1: [ 578, 423, 415] [0:  272, 1:    0, 2: 1333, ]
         2: [ 515, 917,-361] [0: 1605, 1: 1333, 2:    0, ]
        <BLANKLINE>
        """
        returning_str = ""
        print(len(self.scanner_list))
        for scanner in self.scanner_list:
            returning_str += str(scanner) + '\n'

        return returning_str

    def find_path(self, id1, id2, dictionary):
        """
        Finds a path from id2 to id1 using keys in dictionary

        Example:
        >>> space_object = load_data("test_file1.txt")
        >>> space_object.find_path(0, 4, {(0, 1): 1, (1, 3): 4, (3, 2): 5, (2, 4): 6, (2, 3): 9})
        (True, [0, 1, 3, 2, 4])
        """
        assert type(list(dictionary.keys())[0]) == tuple and \
               type(id1) == int and \
               type(id2) == int and \
               len(list(dictionary.keys())[0]) == 2

        path = [id1]
        if (id1, id2) in dictionary:
            path.append(id2)
            return True, path
        for key in list(dictionary.keys()):
            if key[0]==id1:
                dictionary_for_recursion = copy.deepcopy(dictionary)
                if (key[1], id1) in dictionary_for_recursion:
                    dictionary_for_recursion.pop((key[1], id1))
                if (id1, key[1]) in dictionary_for_recursion:
                    dictionary_for_recursion.pop((id1, key[1]))
                found, partial_path = self.find_path(key[1], id2, dictionary_for_recursion)
                if found and len(partial_path) > 1:
                    for next_scanner in partial_path:
                        path.append(next_scanner)
                    return True, path

        return False, path

    def find_orientations_and_translations_to_first_scanner(self):
        """
        Using the property of transitivity, finds orientations of all scanners wrt the first scanner

        Example:
        >>> space_object = load_data("test_file2.txt")
        >>> for i in range(len(space_object.scanner_list)):
        ...     for j in range(len(space_object.scanner_list)):
        ...        if i != j:
        ...            found, mapping, orientation, translation = space_object.scanner_list[i].determine_relationship(space_object.scanner_list[j])
        ...            if found:
        ...                space_object.mapping_dict[(space_object.scanner_list[i].identifier, space_object.scanner_list[j].identifier)] = mapping
        ...                space_object.orientation_dict[(space_object.scanner_list[i].identifier, space_object.scanner_list[j].identifier)] = orientation
        ...                space_object.translation_dict[(space_object.scanner_list[i].identifier, space_object.scanner_list[j].identifier)] = translation
        >>> space_object.find_orientations_and_translations_to_first_scanner()
        >>> for i in range(1, len(space_object.scanner_list)):
        ...     assert (space_object.scanner_list[0].identifier, space_object.scanner_list[i].identifier) in space_object.orientation_dict
        ...     assert (space_object.scanner_list[0].identifier, space_object.scanner_list[i].identifier) in space_object.translation_dict
        >>> assert space_object.orientation_dict[(space_object.scanner_list[0].identifier, space_object.scanner_list[2].identifier)] == [-2, -3, 1]
        >>> assert space_object.translation_dict[(space_object.scanner_list[0].identifier, space_object.scanner_list[2].identifier)] == [20, 1133, -1061]

        """
        for i in range(1, len(self.scanner_list)):
            if (self.scanner_list[0].identifier, self.scanner_list[i].identifier) in self.orientation_dict:
                continue
            else:
                orientation_path = self.find_path(self.scanner_list[0].identifier, self.scanner_list[i].identifier,
                                                  self.orientation_dict)[1]
                orientation = self.orientation_dict[(orientation_path[-2], orientation_path[-1])]
                translation = self.translation_dict[(orientation_path[-2], orientation_path[-1])]
                for j in range(len(orientation_path)-2, 0, -1):
                    orientation = apply_orientation(orientation, self.orientation_dict[(orientation_path[j-1], orientation_path[j])])
                    translation = apply_orientation(translation, self.orientation_dict[(orientation_path[j-1], orientation_path[j])])
                    translation_to_apply_to_translation = [-self.translation_dict[(orientation_path[j-1], orientation_path[j])][k] for k in
                                                           range(len(self.translation_dict[(orientation_path[j-1], orientation_path[j])]))]
                    translation = apply_translation(translation, translation_to_apply_to_translation)
                self.orientation_dict[(self.scanner_list[0].identifier, self.scanner_list[i].identifier)] = orientation
                self.translation_dict[(self.scanner_list[0].identifier, self.scanner_list[i].identifier)] = translation

    def recalibrate(self):
        """
        Recalibrates positions of all beacons with respect to first scanner

        Example:
        >>> space_object = load_data("test_file2.txt")
        >>> for i in range(len(space_object.scanner_list)):
        ...     for j in range(len(space_object.scanner_list)):
        ...        if i != j:
        ...            found, mapping, orientation, translation = space_object.scanner_list[i].determine_relationship(space_object.scanner_list[j])
        ...            if found:
        ...                space_object.mapping_dict[(space_object.scanner_list[i].identifier, space_object.scanner_list[j].identifier)] = mapping
        ...                space_object.orientation_dict[(space_object.scanner_list[i].identifier, space_object.scanner_list[j].identifier)] = orientation
        ...                space_object.translation_dict[(space_object.scanner_list[i].identifier, space_object.scanner_list[j].identifier)] = translation
        >>> space_object.find_orientations_and_translations_to_first_scanner()
        >>> space_object.recalibrate()

        """
        first_scanner = self.scanner_list[0]
        for scanner in self.scanner_list:
            if scanner == first_scanner:
                for beacon in scanner.beacon_list:
                    self.beacons_wrt_first_scanner.add((beacon[0], beacon[1], beacon[2]))
            else:
                for beacon in scanner.beacon_list:
                    new_beacon = copy.deepcopy(beacon)
                    new_beacon = apply_orientation(new_beacon, self.orientation_dict[(first_scanner.identifier, scanner.identifier)])
                    new_beacon = apply_translation(new_beacon, self.translation_dict[(first_scanner.identifier, scanner.identifier)])
                    self.beacons_wrt_first_scanner.add((new_beacon[0], new_beacon[1], new_beacon[2]))

        return

    def count_beacons(self):
        """
        Counts the total number of beacons scanned by all scanners

        Example:

        """
        # For beacon1 in scanner1
        #   For beacon2 in scanner2
        for i in range(len(self.scanner_list)):
            for j in range(len(self.scanner_list)):
                # Ensuring the two scanners are not the same
                if i != j:
                    # Get mapping of all nodes with one other node.
                    found, mapping, orientation, translation = self.scanner_list[i].determine_relationship(self.scanner_list[j])
                    if found:
                        # print(mapping)
                        self.mapping_dict[(self.scanner_list[i].identifier, self.scanner_list[j].identifier)] = mapping
                        self.orientation_dict[(self.scanner_list[i].identifier, self.scanner_list[j].identifier)] = orientation
                        self.translation_dict[(self.scanner_list[i].identifier, self.scanner_list[j].identifier)] = translation
                        # break

        # Find the rotation required to orient all scanners in the same direction as the first scanner using transitivity
        # Find the translation required to move all scanners to the first scanner using the mapping of scanners and
        # transitivity
        self.find_orientations_and_translations_to_first_scanner()

        # Using the rotation and translation information for all scanners, find the beacon coordinates of all beacons with
        # respect to the first scanner adding these to a set of tuples of coordinates. First orient then translate.
        self.recalibrate()

        # for beacon in self.beacons_wrt_first_scanner:
        #     print(str(beacon[0])+','+str(beacon[1])+','+str(beacon[2]))

        return len(self.beacons_wrt_first_scanner)

    def find_greatest_manhatten_distance_between_scanners(self):
        """
        Finds two scanners that are the furthest apart and returns their identifiers
        """
        self.count_beacons()

        scanner_coordinates = {self.scanner_list[0].identifier: [0, 0, 0]}
        for key, val in self.translation_dict.items():
            if key[0] == self.scanner_list[0].identifier:
                scanner_coordinates[key[1]] = val

        max_distance = 0
        first_scanner = 0
        second_scanner = 0
        for i in scanner_coordinates.keys():
            for j in scanner_coordinates.keys():
                if manhattan_distance(scanner_coordinates[i], scanner_coordinates[j])>max_distance:
                    max_distance = manhattan_distance(scanner_coordinates[i], scanner_coordinates[j])
                    first_scanner = i
                    second_scanner = j
        # print(first_scanner)
        # print(second_scanner)

        return max_distance




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

    def determine_relationship(self, scanner_object):
        """
        Determines mapping of beacons, the orientation and translation relation between the two scanners.

        Example:
        >>> space_object = load_data("test_file1.txt")
        >>> space_object.scanner_list[0].determine_relationship(space_object.scanner_list[1])
        (True, {1: 8, 3: 12, 4: 1, 5: 24, 6: 18, 7: 10, 9: 0, 12: 2, 14: 5, 19: 15, 24: 19, 0: 3}, [-1, 2, -3], [-68, 1246, 43])
        """
        assert isinstance(scanner_object, Scanner)

        beacon_mapping = self.find_mapping(scanner_object)
        orientation_relation = []
        translation_relation = []

        if len(beacon_mapping) > 11:
            # Find orientation relation
            orientation_relation = self.find_orientation(scanner_object, beacon_mapping)

            # Find translation relation
            translation_relation = self.find_translation(scanner_object, beacon_mapping, orientation_relation)

            return True, beacon_mapping, orientation_relation, translation_relation
        else:
            return False, beacon_mapping, orientation_relation, translation_relation

    def find_mapping(self, scanner_object):
        """
        Finds whether the two scanners have overlapping scan cubes by finding 12 common beacons.

        Example:
        >>> space_object = load_data("test_file1.txt")
        >>> space_object.scanner_list[0].find_mapping(space_object.scanner_list[1])
        {1: 8, 3: 12, 4: 1, 5: 24, 6: 18, 7: 10, 9: 0, 12: 2, 14: 5, 19: 15, 24: 19, 0: 3}

        """
        assert isinstance(scanner_object, Scanner)
        beacon_mapping_counts = {}

        # Looping through relative distances of each beacon in first scanner
        for i in range(len(self.relative_distances_per_beacon_list)):
            for j in range(len(scanner_object.relative_distances_per_beacon_list)):
                beacon_mapping = {}
                for k in range(len(self.relative_distances_per_beacon_list[i])):
                    if k != i and self.relative_distances_per_beacon_list[i][k] in \
                            scanner_object.relative_distances_per_beacon_list[j]:
                        indexes = [l for l in range(len(scanner_object.relative_distances_per_beacon_list[j])) if
                                   scanner_object.relative_distances_per_beacon_list[j][l] ==
                                   self.relative_distances_per_beacon_list[i][k]]
                        for l in indexes:
                            beacon_mapping[k] = l
                            if (k, l) not in beacon_mapping_counts:
                                beacon_mapping_counts[(k, l)] = 1
                            else:
                                beacon_mapping_counts[(k, l)] += 1

                # if len(beacon_mapping) > 11:
                #     beacon_mapping_list[(i, j)] = beacon_mapping
                #     # print("i: " + str(i) + ",j: " + str(j))
                #     # print(beacon_mapping)

        duplicate_mapping_counts = copy.deepcopy(beacon_mapping_counts)
        for key, val in beacon_mapping_counts.items():
            if val < 11:
                duplicate_mapping_counts.pop(key)
        # print(beacon_mapping_counts)
        # print(duplicate_mapping_counts)

        for key1 in duplicate_mapping_counts.keys():
            for key2 in duplicate_mapping_counts.keys():
                if key1 != key2:
                    if key1[0] == key2[0] or key1[1] == key2[1]:
                        print(
                            "Check this case. One point from one scanner is matching against two points in the second")
                        exit(1)

        final_mapping = {}
        for key in duplicate_mapping_counts:
            final_mapping[key[0]] = key[1]

        return final_mapping

    def find_orientation(self, scanner_object, mapping):
        """
        Finds the orientation required to orient scanner_object in the same direction as the calling scanner.

        Example:
        >>> space_object = load_data("test_file1.txt")
        >>> mapping = space_object.scanner_list[0].find_mapping(space_object.scanner_list[1])
        >>> space_object.scanner_list[0].find_orientation(space_object.scanner_list[1], mapping)
        [-1, 2, -3]
        """
        assert isinstance(scanner_object, Scanner) and len(mapping)>11
        orientation = []

        # Getting the same two beacons from the two scanners
        scanner1_beacon1 = copy.deepcopy(self.beacon_list[list(mapping.keys())[0]])
        scanner1_beacon2 = copy.deepcopy(self.beacon_list[list(mapping.keys())[1]])
        scanner2_beacon1 = copy.deepcopy(scanner_object.beacon_list[list(mapping.values())[0]])
        scanner2_beacon2 = copy.deepcopy(scanner_object.beacon_list[list(mapping.values())[1]])

        # Determining orientation change for 1st coordinate
        if scanner2_beacon1[0]-scanner1_beacon1[0] == scanner2_beacon2[0]-scanner1_beacon2[0]:
            orientation.append(1)
        elif -scanner2_beacon1[0]-scanner1_beacon1[0] == -scanner2_beacon2[0]-scanner1_beacon2[0]:
            orientation.append(-1)
        elif scanner2_beacon1[1]-scanner1_beacon1[0] == scanner2_beacon2[1]-scanner1_beacon2[0]:
            orientation.append(2)
        elif -scanner2_beacon1[1]-scanner1_beacon1[0] == -scanner2_beacon2[1]-scanner1_beacon2[0]:
            orientation.append(-2)
        elif scanner2_beacon1[2]-scanner1_beacon1[0] == scanner2_beacon2[2]-scanner1_beacon2[0]:
            orientation.append(3)
        elif -scanner2_beacon1[2]-scanner1_beacon1[0] == -scanner2_beacon2[2]-scanner1_beacon2[0]:
            orientation.append(-3)

        # Determining orientation change for 2nd coordinate
        if scanner2_beacon1[0]-scanner1_beacon1[1] == scanner2_beacon2[0]-scanner1_beacon2[1]:
            orientation.append(1)
        elif -scanner2_beacon1[0]-scanner1_beacon1[1] == -scanner2_beacon2[0]-scanner1_beacon2[1]:
            orientation.append(-1)
        elif scanner2_beacon1[1]-scanner1_beacon1[1] == scanner2_beacon2[1]-scanner1_beacon2[1]:
            orientation.append(2)
        elif -scanner2_beacon1[1]-scanner1_beacon1[1] == -scanner2_beacon2[1]-scanner1_beacon2[1]:
            orientation.append(-2)
        elif scanner2_beacon1[2]-scanner1_beacon1[1] == scanner2_beacon2[2]-scanner1_beacon2[1]:
            orientation.append(3)
        elif -scanner2_beacon1[2]-scanner1_beacon1[1] == -scanner2_beacon2[2]-scanner1_beacon2[1]:
            orientation.append(-3)

        # Determining orientation change for 3rd coordinate
        if scanner2_beacon1[0]-scanner1_beacon1[2] == scanner2_beacon2[0]-scanner1_beacon2[2]:
            orientation.append(1)
        elif -scanner2_beacon1[0]-scanner1_beacon1[2] == -scanner2_beacon2[0]-scanner1_beacon2[2]:
            orientation.append(-1)
        elif scanner2_beacon1[1]-scanner1_beacon1[2] == scanner2_beacon2[1]-scanner1_beacon2[2]:
            orientation.append(2)
        elif -scanner2_beacon1[1]-scanner1_beacon1[2] == -scanner2_beacon2[1]-scanner1_beacon2[2]:
            orientation.append(-2)
        elif scanner2_beacon1[2]-scanner1_beacon1[2] == scanner2_beacon2[2]-scanner1_beacon2[2]:
            orientation.append(3)
        elif -scanner2_beacon1[2]-scanner1_beacon1[2] == -scanner2_beacon2[2]-scanner1_beacon2[2]:
            orientation.append(-3)

        # Ensuring that the 3 coordinates are different.
        assert len(orientation) == 3 and \
               abs(orientation[0]) != abs(orientation[1]) and \
               abs(orientation[1]) != abs(orientation[2]) and \
               abs(orientation[0]) != abs(orientation[2])

        return orientation

    def find_translation(self, scanner_object, mapping, orientation):
        """
        Finds the translation required to shift origin from scanner_object to calling scanner

        Example:
        >>> space_object = load_data("test_file1.txt")
        >>> mapping = space_object.scanner_list[0].find_mapping(space_object.scanner_list[1])
        >>> orientation = space_object.scanner_list[0].find_orientation(space_object.scanner_list[1], mapping)
        >>> space_object.scanner_list[0].find_translation(space_object.scanner_list[1], mapping, orientation)
        [-68, 1246, 43]
        """
        assert isinstance(scanner_object, Scanner) and len(mapping) > 11

        # Getting the same two beacons from the two scanners and orienting the beacons of second scanner wrt the first
        scanner1_beacon = copy.deepcopy(self.beacon_list[list(mapping.keys())[0]])
        scanner2_beacon = copy.deepcopy(apply_orientation(scanner_object.beacon_list[list(mapping.values())[0]], orientation))

        return [scanner2_beacon[0]-scanner1_beacon[0],
                scanner2_beacon[1]-scanner1_beacon[1],
                scanner2_beacon[2]-scanner1_beacon[2]]

    def check_mapping(self, scanner_object, beacon1, beacon2):
        returning_string = ""
        for beacon in [self.relative_distances_per_beacon_list[beacon1],
                       scanner_object.relative_distances_per_beacon_list[beacon2]]:
            returning_string += "["
            for i in range(len(beacon)):
                returning_string += str(i) + ": " + str(beacon[i]).rjust(4) + ", "
            returning_string += "]\n"

        return returning_string


def apply_orientation(coordinates, orientation):
    """
    Applies orientation and returns new coordinates.

    Example:
    >>> space_object = load_data("test_file1.txt")
    >>> mapping = space_object.scanner_list[0].find_mapping(space_object.scanner_list[1])
    >>> orientation = space_object.scanner_list[0].find_orientation(space_object.scanner_list[1], mapping)
    >>> apply_orientation(space_object.scanner_list[1].beacon_list[8], orientation)
    [460, 603, 452]
    """
    return [-coordinates[abs(orientation[i])-1] if orientation[i] < 0 else coordinates[abs(orientation[i])-1] for i in range(3)]


def apply_translation(coordinates, translation):
    """
    Applies translation and returns new coordinates

    Example:
    >>> space_object = load_data("test_file1.txt")
    >>> mapping = space_object.scanner_list[0].find_mapping(space_object.scanner_list[1])
    >>> orientation = space_object.scanner_list[0].find_orientation(space_object.scanner_list[1], mapping)
    >>> translation = space_object.scanner_list[0].find_translation(space_object.scanner_list[1], mapping, orientation)
    >>> apply_translation(apply_orientation(space_object.scanner_list[1].beacon_list[8], orientation), translation)
    [528, -643, 409]
    """
    return [coordinates[0]-translation[0], coordinates[1]-translation[1], coordinates[2]-translation[2]]

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
                scanner_identifier = int(line.strip().split(' ')[2])
                beacons = []
                continue
            else:
                numbers = line.strip().split(',')
                beacons.append([int(numbers[0]), int(numbers[1]), int(numbers[2])])

    scanner_list.append(Scanner(scanner_identifier, beacons))
    space_object = SpaceScanner(scanner_list)

    return space_object


def main():
    parser = argparse.ArgumentParser(description='AoC Day 19 Beacon Scanner')
    parser.add_argument('-f', '--file',
                        help='Input file with coordinates to the target area grid.',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Find total number of beacons or 2: Find manhatten distance of scanners furthest '
                             'apart.',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    space_object = load_data(arguments.file)

    # print(space_object)

    if arguments.code == 1:
        print(space_object.count_beacons())
    elif arguments.code == 2:
        print(space_object.find_greatest_manhatten_distance_between_scanners())
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
