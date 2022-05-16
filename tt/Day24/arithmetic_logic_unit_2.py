import argparse
from z3 import Int, If, simplify, solve, Optimize


class MONAD(object):
    """
    Class representing the MOdel Number Automatic Detector.

    Attributes
    ----------
    instruction_list : list
        A list of instructions which is the input of this puzzle
    """

    def __init__(self, instruction_list):
        """
        Class initializer

        Parameters
        ----------
        instruction_list : list
            A list of instructions which is the input of this puzzle

        Example:
        >>> object1 = load_data("test1.txt")
        >>> assert len(object1.instruction_list) == 2
        >>> object2 = load_data("test2.txt")
        >>> assert len(object2.instruction_list) == 4
        >>> object3 = load_data("test3.txt")
        >>> assert len(object3.instruction_list) == 11
        """
        self.instruction_list = instruction_list
        self.ALU = (ArithmeticLogicUnit({'w': 0, 'x': 0, 'y': 0, 'z': 0}))

    def __str__(self):
        """
        String representation of this class

        Example:
        >>> object1=load_data("test1.txt")
        >>> print(object1)
        Instructions:
        inp x N/A
        mul x -1
        Registers:
        w: 0
        x: 0
        y: 0
        z: 0
        Counter  : 0
        <BLANKLINE>
        >>> object2=load_data("test2.txt")
        >>> print(object2)
        Instructions:
        inp z N/A
        inp x N/A
        mul z 3
        eql z x
        Registers:
        w: 0
        x: 0
        y: 0
        z: 0
        Counter  : 0
        <BLANKLINE>
        >>> object3=load_data("test3.txt")
        >>> print(object3)
        Instructions:
        inp w N/A
        add z w
        mod z 2
        div w 2
        add y w
        mod y 2
        div w 2
        add x w
        mod x 2
        div w 2
        mod w 2
        Registers:
        w: 0
        x: 0
        y: 0
        z: 0
        Counter  : 0
        <BLANKLINE>
        """
        returning_str = "Instructions:\n"
        for instruction in self.instruction_list:
            returning_str += str(instruction) + '\n'
        returning_str += str(self.ALU)

        return returning_str

    def find_largest_model_number(self):
        """
        Find the largest valid Model Number

        """

        # for i in range(len(self.instruction_list)):
        for i in range(len(self.instruction_list)):
            self.ALU.execute_instruction(self.instruction_list[i])
            # print(str(i) + ':')
            # print(self.ALU)

        print(self.ALU)

        z = Int('z')
        i0 = Int('i0')
        i1 = Int('i1')
        i2 = Int('i2')
        i3 = Int('i3')
        i4 = Int('i4')
        i5 = Int('i5')
        i6 = Int('i6')
        i7 = Int('i7')
        i8 = Int('i8')
        i9 = Int('i9')
        i10 = Int('i10')
        i11 = Int('i11')
        i12 = Int('i12')
        i13 = Int('i13')
        solve(z == self.ALU.register_dict['z'], z == 0,
              # i0 > 0, i0 < 10
              # i1 > 0, i1 < 10,
              # i2 > 0, i2 < 10,
              # i3 > 0, i3 < 10,
              # i4 > 0, i4 < 10,
              # i5 > 0, i5 < 10,
              # i6 > 0, i6 < 10,
              # i7 > 0, i7 < 10,
              # i8 > 0, i8 < 10,
              # i9 > 0, i9 < 10,
              # i10 > 0, i10 < 10,
              # i11 > 0, i11 < 10,
              # i12 > 0, i12 < 10,
              # i13 > 0, i13 < 10,
              )


class ArithmeticLogicUnit(object):
    """
    Class representing the ALU

    Attributes
    ----------
    register_dict : dict
        Dictionary (string -> integer) of register and its stored value
    input_counter : int
        Integer representing the index of this input
    """

    def __init__(self, register_dict):
        """
        Class initializer

        register_dict : dict
            Dictionary (string -> integer) of register and its stored value

        Example:
        >>> object1 = ArithmeticLogicUnit({'w': 0, 'x': 0, 'y': 0, 'z': 0})
        >>> assert object1.register_dict['w'] == 0 and object1.register_dict['x'] == 0 and \
        object1.register_dict['y'] == 0 and object1.register_dict['z'] == 0
        """
        self.register_dict = register_dict
        self.input_counter = 0

    def __str__(self):
        """
        String representation of this class

        Example:
        >>> object1 = ArithmeticLogicUnit({'w': 0, 'x': 0, 'y': 0, 'z': 0})
        >>> print(object1)
        Registers:
        w: 0
        x: 0
        y: 0
        z: 0
        Counter  : 0
        <BLANKLINE>
        """
        returning_str = "Registers:\n"
        for register, value in self.register_dict.items():
            returning_str += register + ': ' + str(value) + '\n'
        returning_str += "Counter  : " + str(self.input_counter) + '\n'

        return returning_str

    def execute_instruction(self, instruction, input="N/A"):
        """
        Executes the Instruction by updating registers.

        Parameters
        ----------
        instruction : Instructions
            Instruction to be executed by this ALU
        input : int
            Integer input to for the 'inp' Instruction

        Example:
        >>> object1 = ArithmeticLogicUnit({'w': 0, 'x': 0, 'y': 0, 'z': 0})
        >>> object1.execute_instruction(Instructions('inp', 'w'))
        >>> assert object1.register_dict['w'] == Int('i0')
        >>> object1.execute_instruction(Instructions('add', 'x', 'w'))
        >>> object1.register_dict['x']
        0 + i0
        >>> object1.execute_instruction(Instructions('mul', 'z', 3))
        >>> object1.register_dict['z']
        0
        >>> object1.execute_instruction(Instructions('div', 'x', 'w'))
        >>> object1.register_dict['x']
        (0 + i0)/i0
        >>> object1.execute_instruction(Instructions('mod', 'w', 3))
        >>> object1.register_dict['w']
        i0%3
        >>> object1.execute_instruction(Instructions('eql', 'y', 'z'))
        >>> object1.register_dict['y']
        If(True, 1, 0)
        >>> object1.execute_instruction(Instructions('eql', 'z', 'w'))
        >>> object1.register_dict['z']
        If(i0%3 == 0, 1, 0)
        """
        if instruction.instruction == "inp":
            self.register_dict[instruction.operand1] = Int('i' + str(self.input_counter))
            self.input_counter += 1
        elif instruction.instruction == "add":
            if isinstance(instruction.operand2, int):
                self.register_dict[instruction.operand1] += instruction.operand2
            else:
                self.register_dict[instruction.operand1] += self.register_dict[instruction.operand2]
        elif instruction.instruction == "mul":
            if isinstance(instruction.operand2, int):
                self.register_dict[instruction.operand1] *= instruction.operand2
            else:
                self.register_dict[instruction.operand1] *= self.register_dict[instruction.operand2]
        elif instruction.instruction == "div":
            if isinstance(instruction.operand2, int):
                self.register_dict[instruction.operand1] /= instruction.operand2
            else:
                self.register_dict[instruction.operand1] /= self.register_dict[instruction.operand2]
        elif instruction.instruction == "mod":
            if isinstance(instruction.operand2, int):
                self.register_dict[instruction.operand1] %= instruction.operand2
            else:
                self.register_dict[instruction.operand1] %= self.register_dict[instruction.operand2]
        elif instruction.instruction == "eql":
            if isinstance(instruction.operand2, int):
                self.register_dict[instruction.operand1] = If(self.register_dict[instruction.operand1] ==
                                                              instruction.operand2, 1, 0)
            else:
                self.register_dict[instruction.operand1] = If(self.register_dict[instruction.operand1] ==
                                                              self.register_dict[instruction.operand2], 1, 0)

        return


class Instructions(object):
    """
    Class representing instructions

    Attributes
    ----------
    instruction : str
        Type of instruction. There are 6 instructions as follows:
            inp: 1 operand. Reads input and writes to variable signified by the operand.
            add: 2 operands. Adds value of second operand to first and stores result in first operand.
            mul: 2 operands. Multiplies values in first and second operands and stores result in first operand.
            div: 2 operands. Divides value of second operand with first and stores result in first operand.
            mod: 2 operands. Divides value of second operand with first and stores remainder in first operand.
            eql: 2 operands. Compares the values of the two operands and stores 1 in operand 1 if equal else 0.
    operand1 : str
        Variable. Result is stored in this result
    operand2 : str/int
        Variable or integer value. Initial value will be "N/A" for inp.
    # symbolic : sympy.core.operation
    #     Symbolic representation of this instruction
    """

    def __init__(self, instruction, operand1, operand2="N/A"):
        """
        Class initializer

        Example:
        >>> inp_instr = Instructions("inp", 'w')
        >>> assert inp_instr.instruction == "inp" and inp_instr.operand1 == 'w' and inp_instr.operand2 == "N/A"
        >>> add_instr = Instructions("add", 'x', 'y')
        >>> assert add_instr.instruction == "add" and add_instr.operand1 == 'x' and add_instr.operand2 == 'y'
        >>> add_instr = Instructions("add", 2, 'y')
        Traceback (most recent call last):
        ...
        AssertionError
        >>> mul_instr = Instructions("mul", 'w', -2)
        >>> assert mul_instr.instruction == "mul" and mul_instr.operand1 == 'w' and mul_instr.operand2 == -2
        >>> div_instr = Instructions("div", 'w', 'z')
        >>> assert div_instr.instruction == "div" and div_instr.operand1 == 'w' and div_instr.operand2 == 'z'
        >>> mod_instr = Instructions("mod", 'w', 'z')
        >>> assert mod_instr.instruction == "mod" and mod_instr.operand1 == 'w' and mod_instr.operand2 == 'z'
        >>> eql_instr = Instructions("eql", 'w', 'z')
        >>> assert eql_instr.instruction == "eql" and eql_instr.operand1 == 'w' and eql_instr.operand2 == 'z'
        """
        assert instruction == "inp" or "add" or "mul" or "div" or "mod" or "eql"
        self.instruction = instruction
        assert isinstance(operand1, str)
        self.operand1 = operand1
        assert isinstance(operand2, str) or isinstance(operand2, int)
        self.operand2 = operand2

    def __str__(self):
        """
        String representation of this class

        Example:
        >>> print(Instructions("inp", 'w'))
        inp w N/A
        >>> print(Instructions("add", 'x', 'y'))
        add x y
        >>> print(Instructions("add", 2, 'y'))
        Traceback (most recent call last):
        ...
        AssertionError
        >>> print(Instructions("mul", 'w', -2))
        mul w -2
        >>> print(Instructions("div", 'w', 'z'))
        div w z
        >>> print(Instructions("mod", 'w', 'z'))
        mod w z
        >>> print(Instructions("eql", 'w', 'z'))
        eql w z
        """
        returning_str = self.instruction + ' ' + self.operand1 + ' ' + str(self.operand2)
        return returning_str


def load_data(file_name):
    instruction_list = []
    with open(file_name, 'r') as f:
        for line in f:
            instruction_components = line.strip().split(' ')
            if len(instruction_components) == 2 and instruction_components[0] == "inp":
                instruction_list.append(Instructions(instruction_components[0],
                                                     instruction_components[1],
                                                     "N/A"))
            elif len(instruction_components) == 3:
                instruction_list.append(Instructions(instruction_components[0],
                                                     instruction_components[1],
                                                     instruction_components[2] if instruction_components[2].isalpha()
                                                     else int(instruction_components[2])))
            else:
                print("Input instruction discrepancy. Check input for correctness.")

    return MONAD(instruction_list)


def main():
    parser = argparse.ArgumentParser(description='AoC Day 24 Arithmetic Logic Unit')
    parser.add_argument('-f', "--file",
                        help="Input file with each line containing a program instruction list.",
                        default="input.txt")
    parser.add_argument('-c', "--code",
                        help="Select 1: Find the largest model number that is valid, 2: ",
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    alu_object = load_data(arguments.file)

    # print(alu_object)

    if arguments.code == 1:
        alu_object.find_largest_model_number()
        # alu_object.substitute_zs()
        # print(alu_object.find_largest_model_number_driver())
    # elif arguments.code == 2:
    #     print(reactor_reboot_object.reboot_reactor(2))
    # else:
    #     print("Selected code not valid")


if __name__ == "__main__":
    main()
