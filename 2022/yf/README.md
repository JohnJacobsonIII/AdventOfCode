# This file is to maintain solutions from Yuyou
## Day1
  + Simple file IO operations. Given a file including lots of numbers seperated by blank lines. You should sum up all consecutive numbers between two blank lines or one blank line and EOF. Let's call the summed number an "element". 
  Finally, you should find the largest 3 element and print the largest one and sum of all 3. 
## Day2 
  + It would be easier to map XYZ to ABC first. Besides you write two functions for obtaining result score and shape score. Finally for part2, you don't need to do much updates. You can add a function by using the match result to modify your shape. This way works compatible with part 1.
## Day3
  + I did bad in this problem. Using unordered_set should be good. I used two int[128] arrays and it made things complicated. You just check the intersection of strings and get sum of their priorities. For part1, the intersection is between the first half and last hald of strings. For part2, the intersction is from 3 strings grouped in the input.
## Day4
  + All you need is two functoins, isIn and overlap, to check if one interval contains the other and one is overlapped with the other. For part 1, you need isIn(a,b) || is isIn(b,a). For part 2, you need isIn(a,b)||isIn(b,a)||overlap(a,b) || overlap(b,a); 
## Day5
  + No much things to say. This is a pure string manipulation problem. Instead of handle the complicated cranes input, you should hard code those strings in your code for speed. The operations just truncate a suffix of a string and append it to another. The difference between part1 and 2 is if we need to reverse the suffix.
## Day6
  + All we need is to maintain a set of distinct chars, and check if the size meets requirement. Still, we can hard code the input string instead of reading from a file
## Day7
  + Maintain a filesystem counting all dirs larger than ceratin size. First, regard file as a dir with 0 items to avoid inheritance. Second, build a vector to save all dir. To express the tree structure, use the indexes in the vector. Third, use an unordered_set to avoid repeated inserting to the vector. 
## Day8
  + Maintain a "prefix" array in 4 directions. Say 0 for row prefix, 1 for row suffix, 2 for col prefix (from top to bottom), 3 for col suffix. By this method, you can eaisly get the tall tree of any directions. 
## Day9
  + Still a silumation problem. Maintain an array of knots. An easy way is mentioned in Day8. Mapping RLTD to 0123. Watch out the direction of knots moving.

## Day10
  + Pure simulation. Just remeber part 1 is "DURING" the cycle and part2 is "AFTER" the cycle.
  
## Day11
  + Just simulate all monkeys. For part2, you should module the product of all test numbers to avoid using big integer.
  
## Day12
  + Do a BFS on the graph with the condition. For the part2, reverse the start and end point and do another BFS with reversed setpping condition. Watch out don't cross border. A tip is to wrap the original graph with a large char (I used 'z' + 3 here). This can help in avoiding board checking in part1. 
