from time import sleep
import os
import argparse
from colorama import Fore


class Parking(object):
    """
    Class representing parking space covered by sea cucumbers

    Attributes
    ----------
    steps: int
        Number of steps
    next_move_direction: int
        Decides which sea cucumbers will move, 0: east-facing sea cucumbers,
                                               1: south-facing sea cucumbers
    parking_grid: list
        A 2D grid containing either '>'-east facing sea cucumber, 'v'-south facing sea cucumber or '.'-empty spot

    Methods
    -------
    check_next_half_step:
        Checks what can change in the next step
    next_half_step:
        Takes the next_half_step

    """
    def __init__(self, parking_grid):
        """
        Class initializer

        Parameters
        ----------
        parking_grid: list
            A 2D grid containing either '>'-east facing sea cucumber, 'v'-south facing sea cucumber or '.'-empty spot

        Example:
        >>> object1 = load_data("test1.txt")

        >>> assert object1.steps == 0
        >>> assert object1.next_move_direction == 0
        >>> assert object1.parking_grid == ['...>...', '.......', '......>', 'v.....>', '......>', '.......', '..vvv..']
        """
        self.steps = 0
        self.next_move_direction = 0
        self.parking_grid = parking_grid

    def __str__(self):
        """
        String representation of this class

        Example:
        >>> object1 = load_data("test1.txt")
        >>> print(object1)
        Steps : 0
        Next  : East
        Grid  :
        ...>...
        .......
        ......>
        v.....>
        ......>
        .......
        ..vvv..
        <BLANKLINE>
        >>> object2 = load_data("test2.txt")
        >>> print(object2)
        Steps : 0
        Next  : East
        Grid  :
        ...>>>>>...
        <BLANKLINE>
        >>> object3 = load_data("test3.txt")
        >>> print(object3)
        Steps : 0
        Next  : East
        Grid  :
        ..........
        .>v....v..
        .......>..
        ..........
        <BLANKLINE>
        """
        returning_str = "Steps".ljust(6) + ": " + str(self.steps) + '\n'
        returning_str += "Next".ljust(6) + ": " + ("South" if self.next_move_direction else "East") + '\n'
        returning_str += "Grid".ljust(6) + ':' + '\n'
        for i in range(len(self.parking_grid)):
            for j in range(len(self.parking_grid[i])):
                returning_str += self.parking_grid[i][j]
            returning_str += '\n'

        return returning_str

    def check_next_half_step(self):
        """
        Checks movement that will take place at the next step.

        Returns
        -------
        String to be printed

        Example:
        >>> object1 = load_data("test1.txt")
        >>> assert type(object1.check_next_half_step()) == str
        >>> object2 = load_data("test2.txt")
        >>> assert type(object2.check_next_half_step()) == str
        >>> object3 = load_data("test3.txt")
        >>> assert type(object3.check_next_half_step()) == str
        """
        returning_str = "Next".ljust(6) + ": " + ("East" if self.next_move_direction else "South") + '\n'
        returning_str += "Grid".ljust(6) + ':' + '\n'
        for i in range(len(self.parking_grid)):
            for j in range(len(self.parking_grid[i])):
                # If we are moving east-facing sea cucumbers
                if not self.next_move_direction:
                    # If spot (i, j) is open to the left we have an east-facing sea cucumber
                    if self.parking_grid[i][j] == '.' and self.parking_grid[i][(j-1) % len(self.parking_grid[i])] == '>':
                        # Move sea cucumber to open space (i, j) by coloring it GREEN
                        returning_str += Fore.GREEN + '>' + Fore.RESET
                    # If spot(i, j) has an east-facing sea cucumber
                    elif self.parking_grid[i][j] == '>':
                        # If the right space is open, free the spot (i,j)
                        if self.parking_grid[i][(j+1) % len(self.parking_grid[i])] == '.':
                            returning_str += '.'
                        else:
                            # The sea cucumber stays in spot (i, j) and is colored RED
                            returning_str += Fore.BLUE + self.parking_grid[i][j] + Fore.RESET
                    else:
                        # Regular print
                        returning_str += self.parking_grid[i][j]
                # If we are moving south-facing sea cucumbers
                elif self.next_move_direction:
                    # If spot (i, j) is open to the top we have a south-facing sea cucumber
                    if self.parking_grid[i][j] == '.' and self.parking_grid[(i-1) % len(self.parking_grid)][j] == 'v':
                        # Move sea cucumber to open space (i, j) by coloring it GREEN
                        returning_str += Fore.GREEN + 'v' + Fore.RESET
                    # If spot(i, j) has a south-facing sea cucumber
                    elif self.parking_grid[i][j] == 'v':
                        # The bottom space is open, free the spot (i, j)
                        if self.parking_grid[(i+1) % len(self.parking_grid)][j] == '.':
                            returning_str += '.'
                        else:
                            # The sea cucumber stays in spot (i, j) and is colored RED
                            returning_str += Fore.BLUE + self.parking_grid[i][j] + Fore.RESET
                    else:
                        # Regular print
                        returning_str += self.parking_grid[i][j]
                else:
                    # Regular print
                    returning_str += self.parking_grid[i][j]
            returning_str += '\n'

        return returning_str

    def next_half_step(self):
        """
        Takes the next half step and adjusts state

        Example:
        >>> object1 = load_data("test1.txt")
        >>> object1.next_half_step()
        3
        >>> object1.next_half_step()
        2
        >>> object2 = load_data("test2.txt")
        >>> object2.next_half_step()
        1
        >>> object2.next_half_step()
        0
        >>> object2.next_half_step()
        2
        >>> object3 = load_data("test3.txt")
        >>> object3.next_half_step()
        1
        >>> object3.next_half_step()
        2
        """
        num_sea_cucumbers_moved = 0
        new_grid = []

        # Generating new grid
        for i in range(len(self.parking_grid)):
            new_grid.append([])

            for j in range(len(self.parking_grid[i])):
                # If we are moving east-facing sea cucumbers
                if not self.next_move_direction:
                    # If spot (i, j) is open to the left we have an east-facing sea cucumber
                    if self.parking_grid[i][j] == '.' and self.parking_grid[i][(j-1) % len(self.parking_grid[i])] == '>':
                        # Move sea cucumber to open space (i, j) by coloring it GREEN
                        new_grid[i].append('>')
                        num_sea_cucumbers_moved += 1
                    # If spot(i, j) has an east-facing sea cucumber
                    elif self.parking_grid[i][j] == '>':
                        # If the right space is open, free the spot (i,j)
                        if self.parking_grid[i][(j+1) % len(self.parking_grid[i])] == '.':
                            new_grid[i].append('.')
                        else:
                            # The sea cucumber stays in spot (i, j) and is colored RED
                            new_grid[i].append(self.parking_grid[i][j])
                    else:
                        # Regular print
                        new_grid[i].append(self.parking_grid[i][j])
                # If we are moving south-facing sea cucumbers
                elif self.next_move_direction:
                    # If spot (i, j) is open to the top we have a south-facing sea cucumber
                    if self.parking_grid[i][j] == '.' and self.parking_grid[(i-1) % len(self.parking_grid)][j] == 'v':
                        # Move sea cucumber to open space (i, j) by coloring it GREEN
                        new_grid[i].append('v')
                        num_sea_cucumbers_moved += 1
                    # If spot(i, j) has a south-facing sea cucumber
                    elif self.parking_grid[i][j] == 'v':
                        # The bottom space is open, free the spot (i, j)
                        if self.parking_grid[(i+1) % len(self.parking_grid)][j] == '.':
                            new_grid[i].append('.')
                        else:
                            # The sea cucumber stays in spot (i, j) and is colored RED
                            new_grid[i].append(self.parking_grid[i][j])
                    else:
                        # Regular print
                        new_grid[i].append(self.parking_grid[i][j])
                else:
                    # Regular print
                    new_grid[i].append(self.parking_grid[i][j])

        # Updating State
        self.parking_grid = new_grid
        self.next_move_direction = 0 if self.next_move_direction else 1

        return num_sea_cucumbers_moved

    def find_steady_state(self):
        """
        Takes the next step till the state cannot be changed anymore

        Example:
        >>> object1 = load_data("test4.txt")
        >>> object1.find_steady_state()
        58
        """
        num_sea_cucumbers_moved_in_this_step = 999
        while num_sea_cucumbers_moved_in_this_step != 0:
            num_sea_cucumbers_moved_in_this_step = 0
            num_sea_cucumbers_moved_in_this_step += self.next_half_step()
            num_sea_cucumbers_moved_in_this_step += self.next_half_step()
            self.steps += 1

        return self.steps

    def animate(self):
        """
        Animates the steps
        """
        num_sea_cucumbers_moved_in_this_step = 999
        while num_sea_cucumbers_moved_in_this_step != 0:
            num_sea_cucumbers_moved_in_this_step = 0
            os.system('clear')
            print(self)
            sleep(0.3)
            num_sea_cucumbers_moved_in_this_step += self.next_half_step()
            os.system('clear')
            print(self)
            sleep(0.3)
            num_sea_cucumbers_moved_in_this_step += self.next_half_step()
            self.steps += 1

        return


def load_data(file_name):
    grid = []
    with open(file_name, 'r') as f:
        for line in f:
            grid.append(line.strip())

    return Parking(grid)


def main():
    parser = argparse.ArgumentParser(description='AoC Day 25 Sea Cucumbers')
    parser.add_argument('-f', "--file",
                        help="Input file with a grid containing south and east facing sea cucumbers.",
                        default="test4.txt")
    parser.add_argument('-c', "--code",
                        help="Select 1: Find the number of steps after which the sea cucumbers stop moving, 2: Animate "
                             "part 1",
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    parking_object = load_data(arguments.file)

    # print(parking_object)

    if arguments.code == 1:
        print(parking_object.check_next_half_step())
    elif arguments.code == 2:
        print(parking_object.animate())
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
