# Common code
# reversed_input_iterator = reversed(input)
# x *= 0
# x += z
# x %= 26
# ... Typically 2 lines
# x = 1 if x == w else 0
# x = 1 if x == 0 else 0
# y *= 0
# y += 25
# y *= x
# y += 1
# z *= y
# y *= 0
# y += w
# ... Typically 1 line
# y *= x
# z += y

# Compressed program
# w = reversed_input_iterator.__next__()
# x = z % 26
#
# z /= values_dividing_z[i]
# x += values_added_to_x[i]
#
# y = 0 if x == w else 26
# x = 0 if x == w else 1
# z *= y
# y = w
#
# y += values_added_to_y[i]
#
# y *= x
# z += y


def input_program(input_number, z):
    reversed_input_iterator = reversed(input_number)
    values_added_to_x = [10, 11, 14, 13, -6, -14, 14, 13, -8, -15, 10, -11, -13, -4]
    values_added_to_y = [1, 9, 12, 6, 9, 15, 7, 12, 15, 3, 6, 2, 10, 12]
    values_dividing_z = [1, 1, 1, 1, 26, 26, 1, 1, 26, 26, 1, 26, 26, 26]

    assert len(values_added_to_x) == len(values_added_to_y) == len(values_dividing_z)

    for i in range(14):
        w = int(reversed_input_iterator.__next__())
        x = (z % 26) + values_added_to_x[i]

        z /= values_dividing_z[i]

        y = 1 if x == w else 26
        x = 0 if x == w else 1
        z *= y
        y = w + values_added_to_y[i]

        y *= x
        z += y # 1 = (z/26)(0 or 26) + (0 or 1)(w+12)
    print("Input: " + input_number)
    return z


def main():
    for input_number in range(99999999999999, 11111111111110, -1):
        if '0' not in str(input_number) and not input_program(str(input_number), 0):
            print(input_number)
            exit(0)


if __name__ == "__main__":
    main()
