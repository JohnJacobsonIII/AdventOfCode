This problem cannot be solved by writing a program for ANY input. Problem needs to be
solved by considered a specific input program.

**Note** This README assumes you have read the puzzle and know details of the 
problem.

# Insights about the given input
1. The given program can be broken down into 14 cycles, each cycle containing 18 
instructions amounting to a total of 252 instructions.
2. The program takes in a 14 digit input number where each digit can be 1 to 9 
(cannot be 0) and goes to adifferent cycle starting with the most significant digit.
3. There are two types of cycles and the program contains 7 of each cycle as follows
    1. Increasing z Cycle where the value in register z strictly increases
    2. Decreasing z Cycle where the value in register z may increase depending on a
       condition.
4. In each cycle
    1. The first instruction takes in a single digit from the input into register w.
    2. x is then set to z % 26. A hardcoded value (negative and greater than -26 
       for decreasing cycles, positive and less than 26 for increasing cycles) is 
       then added to x.
    3. z is divided by 1 in an increasing cycle and by 26 in an increasing cycle.
    4. x is compared with the cycles input and is set to 1 if they are equal and 0
       otherwise.
    5. z is multiplied by 1 or 26 based on if x is 0 or 1.
    6. Then the input value and a distinct hardcoded value for each cycle (positive 
       and less than 26) is added to z on increasing cycles. On decreasing cycles,
       no value is added depending on if x is 0.
5. Because there are 7 increasing z cycles and 7 may decrease z cycles, if in any
   decreasing z cycle, the value does not decrease as much as it can, the value of z
   will not reach 0 at the end of the program.
6. For the value to decrease the most in a cycle, the cycle specific condition that
   sets x to 0 must be satisfied. If all cycle conditions are satisfied, the value 
   of z will be 0 at the end.

# Solution
Simply consider the conditions encountered in the decreasing z cycles and the value 
in register z at the end of every cycle considering initial value is 0.

In an increasing cycle you simply multiply z with 26 and then add the 
cycle specific input along with the hardcoded value that is added to y to z. 
No new condition to add.

In a decreasing cycle,
1. Mod z value by 26 and add cycle specific x-value. Then equate it to the cycles 
   input value (This is the condition you need to satisfy).
2. Divide z value by 26.