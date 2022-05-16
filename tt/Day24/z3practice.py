from z3 import Int, simplify, And, Function, IntSort, solve, If


def main():
    x = Int('x')
    y = Int('y')
    m = Int('m')
    z = m == If(x == y, 1, 0)
    # z = Function('z', IntSort())
    solve(y == x % 4, m == y+2)
    solve(z)
    # solve(z(x + y + 2 * x + 3))
    # print(x)
    # print(Function(x + y + 2 * x + 3))
    # print(simplify(x < y + x + 2))
    # print(simplify(And(x + 1 >= 3, x ** 2 + x ** 2 + y ** 2 + 2 >= 5)))



if __name__ == "__main__":
    main()