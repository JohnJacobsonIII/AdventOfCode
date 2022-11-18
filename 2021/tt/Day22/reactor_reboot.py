import argparse


class ReactorCore(object):
    """
    Class representing 3D space

    Attributes
    ----------
    space_list : list
        A list of cuboid space representing a collection of reactor cubes
    """
    def __init__(self):
        """
        Class initializer

        Parameters
        ----------

        Example:
        >>> object1 = load_data("test1.txt")
        >>> assert not object1.reactor_core.space_list
        """
        self.space_list = []

    def __str__(self):
        """
        String representation of this class

        Example:
        >>> object1 = load_data("test1.txt")
        >>> print(object1.reactor_core)
        Cubes:
        <BLANKLINE>
        """
        returning_str = "Cubes:\n"
        for space in self.space_list:
            for coordinate, interval in space.items():
                returning_str += coordinate + ": [" + str(interval[0]).ljust(6) + ',' + str(interval[1]).ljust(6) + "],"

        return returning_str

    def count_on_cubes(self):
        """
        Counts the number of cubes that are on.

        Returns
        -------
        int
            Number of cubes turned on

        Example:
        >>> object1 = load_data("test1.txt")
        >>> object1.reactor_core.turn_on_cubes_in(object1.reboot_steps[0][1])
        >>> assert object1.reactor_core.count_on_cubes() == 27
        >>> object1.reactor_core.turn_on_cubes_in(object1.reboot_steps[1][1])
        >>> assert object1.reactor_core.count_on_cubes() == 46
        >>> object1.reactor_core.turn_off_cubes_in(object1.reboot_steps[2][1])
        >>> assert object1.reactor_core.count_on_cubes() == 38
        >>> object1.reactor_core.turn_on_cubes_in(object1.reboot_steps[3][1])
        >>> assert object1.reactor_core.count_on_cubes() == 39
        """
        number_of_cubes_turned_on = 0

        for space in self.space_list:
            x_dimension = abs(space['x'][1]-space['x'][0]+1)
            y_dimension = abs(space['y'][1]-space['y'][0]+1)
            z_dimension = abs(space['z'][1]-space['z'][0]+1)
            number_of_cubes_turned_on += x_dimension * y_dimension * z_dimension

        return number_of_cubes_turned_on

    def cube_in_space(self, cube):
        """
        Checks whether the reactor cube is turned on by checking whether its coordinates lie in space_list

        Parameters
        ----------
        cube : dict
            Coordinate of some cube in space.

        Returns
        -------
        bool
            Returns True if it lies in space list and False if not

        Example:
        >>> object1 = load_data("test1.txt")
        >>> object1.reactor_core.turn_on_cubes_in(object1.reboot_steps[0][1])
        >>> assert object1.reactor_core.cube_in_space({'x': 10, 'y': 11, 'z': 12})
        >>> assert not object1.reactor_core.cube_in_space({'x': 9, 'y': 9, 'z': 9})
        """
        for space in self.space_list:
            if space['x'][0] <= cube['x'] <= space['x'][1] and \
               space['y'][0] <= cube['y'] <= space['y'][1] and \
               space['z'][0] <= cube['z'] <= space['z'][1]:
                return True
        return False

    def check_for_overlap_and_resolve(self, space, cuboid):
        """
        Checks and returns overlapping regions if any

        Returns
        -------
        dict
            Dictionary (coordinate -> interval) for 3D space to remove
        list
            list of 3D spaces to add
        Example:
        >>> object1 = load_data("test1.txt")
        >>> object1.reactor_core.turn_on_cubes_in(object1.reboot_steps[0][1])
        >>> object1.reactor_core.check_for_overlap_and_resolve(object1.reactor_core.space_list[0], \
        object1.reboot_steps[1][1])
        ({'x': [10, 12], 'y': [10, 12], 'z': [10, 12]}, [{'x': [10, 10], 'y': [10, 12], 'z': [10, 12]}, {'x': [11, 12], 'y': [10, 10], 'z': [10, 12]}, {'x': [11, 12], 'y': [11, 12], 'z': [10, 10]}])

        """
        spaces_to_add = []
        overlapping_region = {'x': [max(space['x'][0], cuboid['x'][0]), min(space['x'][1], cuboid['x'][1])],
                              'y': [max(space['y'][0], cuboid['y'][0]), min(space['y'][1], cuboid['y'][1])],
                              'z': [max(space['z'][0], cuboid['z'][0]), min(space['z'][1], cuboid['z'][1])]}

        if overlapping_region['x'][0] <= overlapping_region['x'][1] and \
           overlapping_region['y'][0] <= overlapping_region['y'][1] and \
           overlapping_region['z'][0] <= overlapping_region['z'][1]:
            non_overlapping_x = []
            if space['x'][0] < overlapping_region['x'][0]:
                non_overlapping_x.append([space['x'][0], overlapping_region['x'][0]-1])
            if overlapping_region['x'][1] < space['x'][1]:
                non_overlapping_x.append([overlapping_region['x'][1]+1, space['x'][1]])

            non_overlapping_y = []
            if space['y'][0] < overlapping_region['y'][0]:
                non_overlapping_y.append([space['y'][0], overlapping_region['y'][0]-1])
            if overlapping_region['y'][1] < space['y'][1]:
                non_overlapping_y.append([overlapping_region['y'][1]+1, space['y'][1]])

            non_overlapping_z = []
            if space['z'][0] < overlapping_region['z'][0]:
                non_overlapping_z.append([space['z'][0], overlapping_region['z'][0]-1])
            if overlapping_region['z'][1] < space['z'][1]:
                non_overlapping_z.append([overlapping_region['z'][1]+1, space['z'][1]])

            for x_interval in non_overlapping_x:
                spaces_to_add.append({'x': x_interval, 'y': space['y'], 'z': space['z']})

            for y_interval in non_overlapping_y:
                spaces_to_add.append({'x': overlapping_region['x'], 'y': y_interval, 'z': space['z']})

            for z_interval in non_overlapping_z:
                spaces_to_add.append({'x': overlapping_region['x'], 'y': overlapping_region['y'], 'z': z_interval})

            return space, spaces_to_add
        else:
            return {}, []

    def turn_on_cubes_in(self, cuboid):
        """
        Turns on all cubes in cuboid by adding them to the on_cubes set

        Example:
        >>> object1 = load_data("test1.txt")
        >>> object1.reactor_core.turn_on_cubes_in(object1.reboot_steps[0][1])
        >>> for x in range(object1.reboot_steps[0][1]['x'][0], object1.reboot_steps[0][1]['x'][1] + 1):
        ...    for y in range(object1.reboot_steps[0][1]['y'][0], object1.reboot_steps[0][1]['y'][1] + 1):
        ...        for z in range(object1.reboot_steps[0][1]['z'][0], object1.reboot_steps[0][1]['z'][1] + 1):
        ...            assert object1.reactor_core.cube_in_space({'x': x, 'y': y, 'z': z})
        >>> object1.reactor_core.turn_on_cubes_in(object1.reboot_steps[1][1])
        >>> for x in range(object1.reboot_steps[1][1]['x'][0], object1.reboot_steps[1][1]['x'][1] + 1):
        ...    for y in range(object1.reboot_steps[1][1]['y'][0], object1.reboot_steps[1][1]['y'][1] + 1):
        ...        for z in range(object1.reboot_steps[1][1]['z'][0], object1.reboot_steps[1][1]['z'][1] + 1):
        ...            assert object1.reactor_core.cube_in_space({'x': x, 'y': y, 'z': z})
        """
        self.turn_off_cubes_in(cuboid)

        # Merge region with an existing space
        for space in self.space_list:
            if space['x'] == cuboid['x'] and space['y'] == cuboid['y']:
                if space['z'][0]-1 == cuboid['z'][1]:
                    space['z'][0] = cuboid['z'][0]
                    return
                elif space['z'][1]+1 == cuboid['z'][0]:
                    space['z'][1] = cuboid['z'][1]
                    return
            elif space['y'] == cuboid['y'] and space['z'] == cuboid['z']:
                if space['x'][0]-1 == cuboid['x'][1]:
                    space['x'][0] = cuboid['x'][0]
                    return
                elif space['x'][1]+1 == cuboid['x'][0]:
                    space['x'][1] = cuboid['x'][1]
                    return
            elif space['x'] == cuboid['x'] and space['z'] == cuboid['z']:
                if space['y'][0]-1 == cuboid['y'][1]:
                    space['y'][0] = cuboid['y'][0]
                    return
                elif space['y'][1]+1 == cuboid['y'][0]:
                    space['y'][1] = cuboid['y'][1]
                    return

        self.space_list.append(cuboid)

    def turn_off_cubes_in(self, cuboid):
        """
        Removes all cubes in cuboid by removing them from the on_cubes set

        Example:
        >>> object1 = load_data("test1.txt")
        >>> object1.reactor_core.turn_on_cubes_in(object1.reboot_steps[0][1])
        >>> object1.reactor_core.turn_on_cubes_in(object1.reboot_steps[1][1])
        >>> object1.reactor_core.turn_off_cubes_in(object1.reboot_steps[2][1])
        >>> for x in range(object1.reboot_steps[2][1]['x'][0], object1.reboot_steps[2][1]['x'][1] + 1):
        ...    for y in range(object1.reboot_steps[2][1]['y'][0], object1.reboot_steps[2][1]['y'][1] + 1):
        ...        for z in range(object1.reboot_steps[2][1]['z'][0], object1.reboot_steps[2][1]['z'][1] + 1):
        ...            assert not object1.reactor_core.cube_in_space({'x': x, 'y': y, 'z': z})

        """
        spaces_to_remove = []
        spaces_to_add = []

        # Remove overlapping region from existing spaces
        for space in self.space_list:
            space, resolving_spaces = self.check_for_overlap_and_resolve(space, cuboid)
            if space:
                spaces_to_remove.append(space)
            if resolving_spaces:
                spaces_to_add.append(resolving_spaces)

        for space in spaces_to_remove:
            self.space_list.remove(space)

        for space_list in spaces_to_add:
            for space in space_list:
                self.space_list.append(space)

    def turn_off_all_cubes_except_in(self, cuboid):
        """
        Turns off all cubes except the cubes in cuboid

        Example:
        >>> object1 = load_data("test1.txt")
        >>> cuboid = {'x': [11, 11], 'y': [11, 11], 'z': [11, 11]}
        >>> object1.reactor_core.turn_on_cubes_in(object1.reboot_steps[0][1])
        >>> object1.reactor_core.turn_off_all_cubes_except_in(cuboid)
        >>> assert object1.reactor_core.space_list[0]['x'][0]>=cuboid['x'][0] and \
        object1.reactor_core.space_list[0]['x'][1]<=cuboid['x'][1]
        >>> assert object1.reactor_core.space_list[0]['y'][0]>=cuboid['y'][0] and \
        object1.reactor_core.space_list[0]['y'][1]<=cuboid['y'][1]
        >>> assert object1.reactor_core.space_list[0]['z'][0]>=cuboid['z'][0] and \
        object1.reactor_core.space_list[0]['z'][1]<=cuboid['z'][1]

        >>> object2 = load_data("test1.txt")
        >>> cuboid = {'x': [11, 13], 'y': [11, 13], 'z': [11, 13]}
        >>> object2.reactor_core.turn_on_cubes_in(object1.reboot_steps[0][1])
        >>> object2.reactor_core.turn_off_all_cubes_except_in(cuboid)
        >>> assert object1.reactor_core.space_list[0]['x'][0]>=cuboid['x'][0] and \
        object1.reactor_core.space_list[0]['x'][1]<=cuboid['x'][1]
        >>> assert object1.reactor_core.space_list[0]['y'][0]>=cuboid['y'][0] and \
        object1.reactor_core.space_list[0]['y'][1]<=cuboid['y'][1]
        >>> assert object1.reactor_core.space_list[0]['z'][0]>=cuboid['z'][0] and \
        object1.reactor_core.space_list[0]['z'][1]<=cuboid['z'][1]
        """
        spaces_to_remove = []
        for i in range(len(self.space_list)):
            overlapping_region = {'x': [max(self.space_list[i]['x'][0], cuboid['x'][0]),
                                        min(self.space_list[i]['x'][1], cuboid['x'][1])],
                                  'y': [max(self.space_list[i]['y'][0], cuboid['y'][0]),
                                        min(self.space_list[i]['y'][1], cuboid['y'][1])],
                                  'z': [max(self.space_list[i]['z'][0], cuboid['z'][0]),
                                        min(self.space_list[i]['z'][1], cuboid['z'][1])]}
            if overlapping_region['x'][0] <= overlapping_region['x'][1] and \
               overlapping_region['y'][0] <= overlapping_region['y'][1] and \
               overlapping_region['z'][0] <= overlapping_region['z'][1]:
                if self.space_list[i] != overlapping_region:
                    self.space_list[i] = overlapping_region
            else:
                spaces_to_remove.append(self.space_list[i])

        for space in spaces_to_remove:
            self.space_list.remove(space)


class ReactorReboot(object):
    """
    Class representing the Reactor Boot process

    Attributes
    ----------
    reboot_steps : list
        Contains the reboot steps to turn the reactor on. A list of tuple of 0/1 and intervals for x,y,z.
    reactor_core = ReactorCore
        Represents an abstraction of a collection of reactor cubes
    grid = dict
        Dictionary (coordinate -> interval) of dimensions of 3D grid
    """
    def __init__(self, reboot_steps, grid):
        """
        Class initializer

        Parameters
        ----------
        reboot_steps: list
            Contains the reboot steps to turn the reactor on. A list of tuple of 0/1 and intervals for x,y,z.

        Example:
        >>> object1 = load_data("test1.txt")
        >>> assert object1.reboot_steps[0][0] and object1.reboot_steps[1][0] and not object1.reboot_steps[2][0] and \
        object1.reboot_steps[3][0]
        >>> assert object1.reboot_steps[0][1]['x'] == [10, 12] and object1.reboot_steps[0][1]['y'] == [10, 12] and \
        object1.reboot_steps[0][1]['z'] == [10, 12]
        >>> assert object1.reboot_steps[1][1]['x'] == [11, 13] and object1.reboot_steps[1][1]['y'] == [11, 13] and \
        object1.reboot_steps[1][1]['z'] == [11, 13]
        >>> assert object1.reboot_steps[2][1]['x'] == [9, 11] and object1.reboot_steps[2][1]['y'] == [9, 11] and \
        object1.reboot_steps[2][1]['z'] == [9, 11]
        >>> assert object1.reboot_steps[3][1]['x'] == [10, 10] and object1.reboot_steps[3][1]['y'] == [10, 10] and \
        object1.reboot_steps[3][1]['z'] == [10, 10]
        >>> assert len(object1.reactor_core.space_list) == 0

        >>> object2 = load_data("test2.txt")
        >>> assert len(object2.reboot_steps) == 22
        >>> assert len(object2.reactor_core.space_list) == 0
        """
        self.reboot_steps = reboot_steps
        self.reactor_core = ReactorCore()
        self.grid = grid

    def __str__(self):
        """
        String representation of the class

        Example:
        >>> object1 = load_data("test1.txt")
        >>> print(object1)
        Grid: {'x': (-50, 50), 'y': (-50, 50), 'z': (-50, 50)}
        Reboot steps:
        On  x: [10    ,12    ],y: [10    ,12    ],z: [10    ,12    ],
        On  x: [11    ,13    ],y: [11    ,13    ],z: [11    ,13    ],
        Off x: [9     ,11    ],y: [9     ,11    ],z: [9     ,11    ],
        On  x: [10    ,10    ],y: [10    ,10    ],z: [10    ,10    ],
        Cubes:
        <BLANKLINE>

        >>> object2 = load_data("test2.txt")
        >>> print(object2)
        Grid: {'x': (-50, 50), 'y': (-50, 50), 'z': (-50, 50)}
        Reboot steps:
        On  x: [-20   ,26    ],y: [-36   ,17    ],z: [-47   ,7     ],
        On  x: [-20   ,33    ],y: [-21   ,23    ],z: [-26   ,28    ],
        On  x: [-22   ,28    ],y: [-29   ,23    ],z: [-38   ,16    ],
        On  x: [-46   ,7     ],y: [-6    ,46    ],z: [-50   ,-1    ],
        On  x: [-49   ,1     ],y: [-3    ,46    ],z: [-24   ,28    ],
        On  x: [2     ,47    ],y: [-22   ,22    ],z: [-23   ,27    ],
        On  x: [-27   ,23    ],y: [-28   ,26    ],z: [-21   ,29    ],
        On  x: [-39   ,5     ],y: [-6    ,47    ],z: [-3    ,44    ],
        On  x: [-30   ,21    ],y: [-8    ,43    ],z: [-13   ,34    ],
        On  x: [-22   ,26    ],y: [-27   ,20    ],z: [-29   ,19    ],
        Off x: [-48   ,-32   ],y: [26    ,41    ],z: [-47   ,-37   ],
        On  x: [-12   ,35    ],y: [6     ,50    ],z: [-50   ,-2    ],
        Off x: [-48   ,-32   ],y: [-32   ,-16   ],z: [-15   ,-5    ],
        On  x: [-18   ,26    ],y: [-33   ,15    ],z: [-7    ,46    ],
        Off x: [-40   ,-22   ],y: [-38   ,-28   ],z: [23    ,41    ],
        On  x: [-16   ,35    ],y: [-41   ,10    ],z: [-47   ,6     ],
        Off x: [-32   ,-23   ],y: [11    ,30    ],z: [-14   ,3     ],
        On  x: [-49   ,-5    ],y: [-3    ,45    ],z: [-29   ,18    ],
        Off x: [18    ,30    ],y: [-20   ,-8    ],z: [-3    ,13    ],
        On  x: [-41   ,9     ],y: [-7    ,43    ],z: [-33   ,15    ],
        On  x: [-54112,-39298],y: [-85059,-49293],z: [-27449,7877  ],
        On  x: [967   ,23432 ],y: [45373 ,81175 ],z: [27513 ,53682 ],
        Cubes:
        <BLANKLINE>
        """
        returning_str = "Grid: " + str(self.grid) + '\n'
        returning_str += "Reboot steps:\n"
        for step in self.reboot_steps:
            returning_str += 'On'.ljust(4) if step[0] else 'Off'.ljust(4)
            for coordinate, interval in step[1].items():
                returning_str += coordinate + ": [" + str(interval[0]).ljust(6) + ',' + str(interval[1]).ljust(6) + "],"
            returning_str += '\n'

        returning_str += self.reactor_core.__str__()

        return returning_str

    def reboot_reactor(self, code):
        """
        Counts the number of cubes that have been turned on

        Example:
        >>> object1 = load_data("test1.txt")
        >>> object1.reboot_reactor(1)
        39
        >>> object2 = load_data("test2.txt")
        >>> object2.reboot_reactor(1)
        590784
        >>> object3 = load_data("test3.txt")
        >>> object3.reboot_reactor(1)
        474140
        >>> object3 = load_data("test3.txt")
        >>> object3.reboot_reactor(2)
        2758514936282235
        """
        for on, cuboid in self.reboot_steps:
            if on:
                self.reactor_core.turn_on_cubes_in(cuboid)
            elif not on:
                self.reactor_core.turn_off_cubes_in(cuboid)

        if code == 1:
            self.reactor_core.turn_off_all_cubes_except_in(self.grid)
        return self.reactor_core.count_on_cubes()


def load_data(file_name):
    reboot_steps = []
    with open(file_name, 'r') as f:
        for line in f:
            instruction, cuboid = line.strip().split(' ')
            cuboid = cuboid.split(',')
            cuboid_coords = {coordinate[0]: [int(coordinate[2:].split('..')[0]), int(coordinate[2:].split('..')[1])] for
                             coordinate in cuboid}
            reboot_steps.append((True if instruction=="on" else False, cuboid_coords))

    return ReactorReboot(reboot_steps, {'x': (-50, 50), 'y': (-50, 50), 'z': (-50, 50)})


def main():
    parser = argparse.ArgumentParser(description='AoC Day 22 Reactor Reboot')
    parser.add_argument('-f', "--file",
                        help="Input file with each line containing instruction to turn cubes on/off and a cuboid of"
                             "of coordinates for cubes to perform this step on.",
                        default="input.txt")
    parser.add_argument('-c', "--code",
                        help="Select 1: Find number of cubes switched on within initialization region 2: Find number "
                             "of cubes switched on",
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    reactor_reboot_object = load_data(arguments.file)

    # print(reactor_reboot_object)

    if arguments.code == 1:
        print(reactor_reboot_object.reboot_reactor(1))
    elif arguments.code == 2:
        print(reactor_reboot_object.reboot_reactor(2))
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
