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
    


if __name__ == '__main__':
    main()
