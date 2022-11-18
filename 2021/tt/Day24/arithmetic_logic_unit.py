import argparse
from copy import deepcopy
from sympy import var, Eq, Piecewise


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
        >>> assert len(object1.instruction_list) == 2 and len(object1.execution_list) == 1
        >>> object2 = load_data("test2.txt")
        >>> assert len(object2.instruction_list) == 4 and len(object2.execution_list) == 1
        >>> object3 = load_data("test3.txt")
        >>> assert len(object3.instruction_list) == 11 and len(object3.execution_list) == 1
        """
        self.instruction_list = instruction_list
        self.execution_list = [(ArithmeticLogicUnit({'w': 0, 'x': 0, 'y': 0, 'z': 0}), 0, '')]

    def __str__(self):
        """
        String representation of this class

        Example:
        >>> object1=load_data("test1.txt")
        >>> print(object1)
        Instructions:
        inp x N/A
        mul x -1
        Program Counter: 0
        Input String:
        Registers:
        w: 0
        x: 0
        y: 0
        z: 0
        <BLANKLINE>
        >>> object2=load_data("test2.txt")
        >>> print(object2)
        Instructions:
        inp z N/A
        inp x N/A
        mul z 3
        eql z x
        Program Counter: 0
        Input String:
        Registers:
        w: 0
        x: 0
        y: 0
        z: 0
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
        Program Counter: 0
        Input String:
        Registers:
        w: 0
        x: 0
        y: 0
        z: 0
        <BLANKLINE>
        """
        returning_str = "Instructions:\n"
        for instruction in self.instruction_list:
            returning_str += str(instruction) + '\n'

        for execution, program_counter, input_string in self.execution_list:
            returning_str += "Program Counter: " + str(program_counter) + '\n'
            returning_str += "Input String:" + input_string + '\n'
            returning_str += str(execution)

        return returning_str

    def parse_program(self):
        """

        """
        current_execution = self.execution_list.pop(0)[0]

        # Loop till all instructions are covered.
        for i in range(len(self.instruction_list)):
        # for i in range(18):
            if self.instruction_list[i].instruction == "inp":

                self.execution_list.append((deepcopy(current_execution), i, ''))
                for register in current_execution.register_dict.keys():
                    current_execution.register_dict[register] = var(register)
                # current_execution.register_dict[self.instruction_list[i].operand1] = var(self.instruction_list[i].operand1)
            else:
                assert self.instruction_list[i].instruction != "inp"
                current_execution.execute_instruction(self.instruction_list[i])
            # print(i + 1)
            # print(current_execution)

        self.execution_list.append((deepcopy(current_execution), len(self.instruction_list), ''))

        # evaluated_list = []
        # for input_value in range(1,10):
        #     evaluating_execution = deepcopy(current_execution)
        #
        #     for register, value in evaluating_execution.register_dict.items():
        #         if not isinstance(value, int):
        #             evaluating_execution.register_dict[register] = value.subs(var('w'), input_value)
        #
        #     evaluated_list.append(evaluating_execution)
        #     print(evaluating_execution)

        for execution, program_counter, program_input in self.execution_list:
            print(execution)

    def substitute_zs(self):
        """

        """
        for i in range(1, len(self.execution_list)):
            z = var('z')
            self.execution_list[i][0].register_dict['z'] = self.execution_list[i][0].register_dict['z']\
                .subs(z, self.execution_list[i-1][0].register_dict['z'])

        print("Printing substituted z ALUS:\n")
        for execution, program_counter, program_input in self.execution_list:
            print(execution)


    def find_largest_model_number(self, previous_execution, index):
        """
        Find the largest valid Model Number

        """
        # Base case
        if index == 15:
            if previous_execution.register_dict['z'] == 0:
                return True, '0'
            else:
                return False, '0'

        for input_value in range(9, 0, -1):
            w = var('w')
            x = var('x')
            y = var('y')
            z = var('z')
            current_execution, program_counter, input_string = deepcopy(self.execution_list[index])
            for register in current_execution.register_dict.keys():
                current_execution.register_dict[register] = \
                    current_execution.register_dict[register]\
                        .subs(w, input_value)\
                        .subs(x, previous_execution.register_dict[register])\
                        .subs(y, previous_execution.register_dict[register])\
                        .subs(z, previous_execution.register_dict[register])
            solved, solution_input_value = self.find_largest_model_number(current_execution, index+1)

            if solved:
                return True, str(input_value) + solution_input_value

        return False, '0'

    def find_largest_model_number_driver(self):
        """

        """
        solved, solution_input_value = self.find_largest_model_number(self.execution_list[0][0], 1)
        return solution_input_value

    # def find_largest_model_number_driver(self):
    #     """
    #     Find the largest valid Model Number
    #
    #     Example:
    #     >>> object1 = load_data("test1.txt")
    #     >>> object1.find_largest_model_number_driver()
    #     '9'
    #     >>> object2 = load_data("test2.txt")
    #     >>> object2.find_largest_model_number_driver()
    #     '99'
    #     >>> object3 = load_data("test3.txt")
    #     >>> object3.find_largest_model_number_driver()
    #     '8'
    #     """
    #     # Loop till no more states left
    #     while self.execution_list != 0:
    #         current_execution, program_counter, input_string = self.execution_list.pop(0)
    #
    #         while program_counter < len(self.instruction_list) and \
    #                 self.instruction_list[program_counter].instruction != "inp":
    #             current_execution.execute_instruction(self.instruction_list[program_counter],
    #                                                   program_counter)
    #             program_counter += 1
    #         if program_counter < len(self.instruction_list):
    #             for input_value in range(1, 10):
    #                 duplicate_execution = deepcopy(current_execution)
    #                 duplicate_execution.execute_instruction(self.instruction_list[program_counter], input_value)
    #
    #                 self.execution_list.insert(0, (duplicate_execution, program_counter+1, input_string+str(input_value)))
    #             continue
    #         elif program_counter == len(self.instruction_list):
    #             if current_execution.register_dict['z'] == 0:
    #                 return input_string


class ArithmeticLogicUnit(object):
    """
    Class representing the ALU

    Attributes
    ----------
    register_dict : dict
        Dictionary (string -> integer) of register and its stored value
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
        <BLANKLINE>
        """
        returning_str = "Registers:\n"
        for register, value in self.register_dict.items():
            returning_str += register + ': ' + str(value) + '\n'

        return returning_str
    
    def execute_instruction(self, instruction, input = "N/A"):
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
        >>> object1.execute_instruction(Instructions('inp', 'w'), 4)
        >>> assert object1.register_dict['w'] == 4
        >>> object1.execute_instruction(Instructions('add', 'x', 'w'))
        >>> assert object1.register_dict['x'] == 4
        >>> object1.execute_instruction(Instructions('mul', 'z', 3))
        >>> assert object1.register_dict['z'] == 0
        >>> object1.execute_instruction(Instructions('div', 'x', 'w'))
        >>> assert object1.register_dict['x'] == 1
        >>> object1.execute_instruction(Instructions('mod', 'w', 3))
        >>> assert object1.register_dict['w'] == 1
        >>> object1.execute_instruction(Instructions('eql', 'y', 'z'))
        >>> assert object1.register_dict['y'] == 1
        >>> object1.execute_instruction(Instructions('eql', 'z', 'w'))
        >>> assert object1.register_dict['z'] == 0
        """
        if instruction.instruction == "inp":
            assert isinstance(input, int)
            self.register_dict[instruction.operand1] = input
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
                if isinstance(self.register_dict[instruction.operand1], int):
                    self.register_dict[instruction.operand1] = 1 if self.register_dict[instruction.operand1] == \
                                                                instruction.operand2 else 0
                elif self.register_dict[instruction.operand1].free_symbols:
                    self.register_dict[instruction.operand1] = Piecewise((1,  Eq(self.register_dict[instruction.operand1],
                                                                                 instruction.operand2)),
                                                                         (0, True))
            else:
                self.register_dict[instruction.operand1] = Piecewise((1,  Eq(self.register_dict[instruction.operand1],
                                                                             self.register_dict[instruction.operand2])),
                                                                     (0, True))

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
        alu_object.parse_program()
        # alu_object.substitute_zs()
        # print(alu_object.find_largest_model_number_driver())
    # elif arguments.code == 2:
    #     print(reactor_reboot_object.reboot_reactor(2))
    # else:
    #     print("Selected code not valid")


if __name__ == "__main__":
    main()

# Constraints
#