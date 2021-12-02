import argparse

def first(data):
    inc_count = sum((1 if pair[0] < pair[1] else 0 for pair in zip(data[:-1],data[1:])))
    print('inc count:', inc_count)
    

def second(data):
    sums = list(map(sum, zip(data[:-2],data[1:-1],data[2:])))
    
    inc_count = sum((1 if pair[0] < pair[1] else 0 for pair in zip(sums[:-1],sums[1:])))
    print('inc count:', inc_count)
    

def main():
    parser = argparse.ArgumentParser(description='Advent of Code 2021 Day 1')
    parser.add_argument('-f', '--file',
                        help='Input file',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='1 or 2, indicates which Advent of Code exercise ',
                        type=int,
                        default=1)

    args = parser.parse_args()
    
    infile = args.file
    with open(infile) as f:
        nums = tuple(map(lambda x: int(x), f.readlines()))

    if args.code == 1:
        first(nums)
    elif args.code == 2:
        second(nums)
    else:
        raise Exception("invalid code; use 1 or 2")


if __name__ == '__main__':
    main()
