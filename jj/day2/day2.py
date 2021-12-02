import argparse

def first(infile):
    # Tanmay's version
    horizontal_distance = 0
    depth = 0
    
    with open(infile) as f:
        for line in f:
            if line.split()[0] == "forward":
                horizontal_distance += int(line.split()[1])
            elif line.split()[0] == "down":
                depth += int(line.split()[1])
            elif line.split()[0] == "up":
                depth -= int(line.split()[1])
    
    print(horizontal_distance)
    print(depth)
    print(horizontal_distance * depth)
    
    # John's version
    horizontal_distance = 0
    depth = 0
    
    with open(infile) as f:
        for line in f:
            strings = line.split()
            if strings[0] == "forward":
                horizontal_distance += int(strings[1])
            elif strings[0] == "down":
                depth += int(strings[1])
            elif strings[0] == "up":
                depth -= int(strings[1])
    
    print('x:', horizontal_distance)
    print('-y:', depth)
    print('prod:', horizontal_distance * depth)
    

def second(infile):
    horizontal_distance = 0
    depth = 0
    aim = 0
    
    with open(infile) as f:
        for line in f:
            direction, distance = line.split()
            distance = int(distance)
            if direction == "forward":
                horizontal_distance += distance
                depth += aim * distance
            elif direction == "down":
                aim   += distance
            elif direction == "up":
                aim   -= distance
    
    print('x:', horizontal_distance)
    print('-y:', depth)
    print('aim:', aim)
    print('prod:', horizontal_distance * depth)


def main():
    parser = argparse.ArgumentParser(description='Advent of Code 2021 Day 2')
    parser.add_argument('-f', '--file',
                        help='Input file',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='1 or 2, indicates which Advent of Code exercise ',
                        type=int,
                        default=1)

    args = parser.parse_args()
    
    if args.code == 1:
        first(args.file)
    elif args.code == 2:
        second(args.file)
    else:
        raise Exception("invalid code; use 1 or 2")
    


if __name__ == '__main__':
    main()
