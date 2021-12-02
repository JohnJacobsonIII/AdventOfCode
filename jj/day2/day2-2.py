def main():
    horizontal_distance = 0
    depth = 0
    
    infile = 'input.txt'
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
    


if __name__ == '__main__':
    main()
