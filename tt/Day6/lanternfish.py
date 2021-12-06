import argparse


def load_data(file_name):
    fish_spawn_routine = [0] * 9
    with open(file_name, 'r') as f:
        data = list(map(int, f.readline().split(',')))
    for number in data:
        fish_spawn_routine[number] += 1
    return fish_spawn_routine


def get_new_fish_spawn_routine(day, fish_spawn_routine):
    temp = fish_spawn_routine[8]
    fish_spawn_routine[8] = fish_spawn_routine[day]
    fish_spawn_routine[day] += fish_spawn_routine[7]
    fish_spawn_routine[7] = temp
    # print(fish_spawn_routine)
    return


def get_new_fish_spawn_routine_n_days(days, fish_spawn_routine):
    for i in range(days):
        get_new_fish_spawn_routine(i % 7, fish_spawn_routine)

    return sum(fish_spawn_routine)


def main():
    parser = argparse.ArgumentParser(description='AoC Day 1 Sonar Sweeper')
    parser.add_argument('-f', '--file',
                        help='Input file with a single line containing estimated times to spawn new lanternfish'
                             'for each lantern fish',
                        default='input.txt')
    arguments = parser.parse_args()

    fish_spawn_routine = load_data(arguments.file)

    print(get_new_fish_spawn_routine_n_days(256, fish_spawn_routine))


if __name__ == "__main__":
    main()
