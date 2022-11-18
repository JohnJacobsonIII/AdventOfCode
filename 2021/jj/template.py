import argparse

def main():
    parser = argparse.ArgumentParser(description='Template for generic python script')
    parser.add_argument('-f', '--file',
                        help='Input file',
                        default='input.txt')
    args = parser.parse_args()

    raise NotImplementedError

if __name__ == '__main__':
    main()
