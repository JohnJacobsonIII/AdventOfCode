def main():
    infile = 'input.txt'
    with open(infile) as f:
        nums = tuple(map(lambda x: int(x), f.readlines()))
    
    sums = list(map(sum, zip(nums[:-2],nums[1:-1],nums[2:])))
    
    inc_count = sum((1 if pair[0] < pair[1] else 0 for pair in zip(sums[:-1],sums[1:])))
    print('inc count:', inc_count)


if __name__ == '__main__':
    main()
