import argparse
import itertools

packet_metadata = []


def load_data(file_name):
    target_area = []
    with open(file_name, 'r') as f:
        line = f.readline().strip().split()
        target_area.append(int(line[2].split('=')[1].split('..')[0]))
        target_area.append(int(line[2].split('=')[1].split('..')[1].split(',')[0]))
        target_area.append(int(line[3].split('=')[1].split('..')[0]))
        target_area.append(int(line[3].split('=')[1].split('..')[1]))

    return target_area


# Create a set of tuples of two ints (These represent the initial x and y velocities)
# Create a dictionary of #steps->sets of x velocities that make the target area in #steps
# For each x_vel from vel = max_x(target_area):min_x(target_area)
#   Find which #steps set the x_vel can be added to
#
# Same step y
# Create a dictionary of #steps->sets of y velocities that make the target area in #steps
# For each y_vel from vel = max_y(target_area):min_y(target_area)
#   Find which #steps set the y_vel can be added to
#
# Take a cross product of the sets having same #step in x and y dictionaries and add to initial_velocities tuple set.
#
# Different step y
# Find x velocities that make probe stop in x_target_zone
#
# Take a cross product of this set and y dictionaries that have #steps more than the x_values and add to initial_
# velocities tuple set.
def all_initial_velocities(target_area):
    # Create a set of tuples of two ints (These represent the initial x and y velocities)
    initial_velocities = set()

    # Create a dictionary of #steps->sets of x velocities that make the target area in #steps
    # For each x_vel from vel = max_x(target_area):min_x(target_area)
    #   Find which #steps set the x_vel can be added to
    x_velocity_dict = {}
    for x_vel in range(target_area[1], 0, -1):
        steps = 0
        x_pos = 0
        while x_vel-steps > 0 and x_pos+x_vel-steps <= target_area[1]:
            x_pos += x_vel - steps
            steps += 1
            if x_pos >= target_area[0]:
                if steps not in x_velocity_dict:
                    x_velocity_dict[steps] = set()
                x_velocity_dict[steps].add(x_vel)

    # print("x_velocities:")
    # for key in x_velocity_dict.keys():
    #     print(key, ": ", x_velocity_dict[key])

    # Direct throw - y velocities are negative
    # Create a dictionary of #steps->sets of y velocities that make the target area in #steps
    # For each y_vel from vel = max_y(target_area):min_y(target_area)
    #   Find which #steps set the y_vel can be added to

    y_velocity_dict = {}
    for y_vel in range(abs(target_area[2]), target_area[2]-1, -1):
        steps = 0
        y_pos = 0
        while y_pos+y_vel-steps >= target_area[2]:
            y_pos += y_vel - steps
            steps += 1
            if y_pos <= target_area[3]:
                if steps not in y_velocity_dict:
                    y_velocity_dict[steps] = set()
                y_velocity_dict[steps].add(y_vel)

    # print("y_velocities:")
    # for key in y_velocity_dict.keys():
    #     print(key, ": ", y_velocity_dict[key])

    # Take a cross product of the sets having same #step in x and y dictionaries and add to initial_velocities tuple
    # set.
    for steps in x_velocity_dict.keys():
        if steps in y_velocity_dict.keys():
            for initial_vel_tup in itertools.product(x_velocity_dict[steps], y_velocity_dict[steps]):
                initial_velocities.add(initial_vel_tup)

    # Parabolic throw - y velocities are non-negative
    # Create a dictionary of #steps->sets of y velocities that make the target area in #steps
    # For each y_vel from vel = max_y(target_area):min_y(target_area)
    #   Find which #steps set the y_vel can be added to
    zeroing_x_vels = set()
    x_pos = 0
    vel = 0
    while x_pos+vel+1 <= target_area[1]:
        x_pos += vel + 1
        vel += 1
        if x_pos >= target_area[0]:
            zeroing_x_vels.add(vel)

    # print("zeroing_x_vels", zeroing_x_vels)

    for vel in zeroing_x_vels:
        for steps in y_velocity_dict.keys():
            if steps >= vel:
                for initial_vel_tup in itertools.product({vel}, y_velocity_dict[steps]):
                    initial_velocities.add(initial_vel_tup)

    return initial_velocities


def print_trajectory(target_area, vx, vy):
    y_max = int(vy*vy/2)
    grid = [['.'] * target_area[1] for y in range(abs(target_area[2])+y_max+1)]
    for line in grid:
        print(line)

    for x in range(target_area[0], target_area[1]+1):
        for y in range(y_max+abs(target_area[3]), y_max+abs(target_area[2])+1):
            grid[y][x] = 'T'


def main():
    parser = argparse.ArgumentParser(description='AoC Day 17 Trick Shot')
    parser.add_argument('-f', '--file',
                        help='Input file with coordinates to the target area grid.',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Find maximum y position or 2: Find all initial velocities.',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    target_area = load_data(arguments.file)
    print(target_area)
    # print_trajectory(target_area, 0, 0)
    if arguments.code == 1:
        print(abs(target_area[2])*(abs(target_area[2])-1)/2)
    elif arguments.code == 2:
        initial_velocities = all_initial_velocities(target_area)
        # print(initial_velocities)
        print(len(initial_velocities))
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
