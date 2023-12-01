# Attempt at Day 1

[Description](https://adventofcode.com/2023/day/1)


## Repeated here

--- __Day 1: Trebuchet?!__ ---

Something is wrong with global snow production, and you've been selected to take a look. The Elves have even given you a map; on it, they've used stars to mark the top fifty locations that are likely to be having problems.

You've been doing this long enough to know that to restore snow operations, you need to check all __fifty stars__ by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants __one star__. Good luck!

You try to ask why they can't just use a [weather machine](https://adventofcode.com/2015/day/1) ("not powerful enough") and where they're even sending you ("the sky") and why your map looks mostly blank ("you sure ask a lot of questions") and hang on did you just say the sky ("of course, where do you think snow comes from") when you realize that the Elves are already loading you into a [trebuchet](https://en.wikipedia.org/wiki/Trebuchet) ("please hold still, we need to strap you in").

As they're making the final adjustments, they discover that their calibration document (your puzzle input) has been __amended__ by a very young Elf who was apparently just excited to show off her art skills. Consequently, the Elves are having trouble reading the values on the document.

The newly-improved calibration document consists of lines of text; each line originally contained a specific __calibration value__ that the Elves now need to recover. On each line, the calibration value can be found by combining the __first digit__ and the __last digit__ (in that order) to form a single __two-digit number__.

For example:

```
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
```

In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces __142__.

Consider your entire calibration document. __What is the sum of all of the calibration values?__

--- __Part Two__ ---

Your calculation isn't quite right. It looks like some of the digits are actually __spelled out with letters:__ one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".

Equipped with this new information, you now need to find the real first and last digit on each line. For example:

```
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
```

In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces __281__.

__What is the sum of all of the calibration values?__

# File manifest:

* __data__: input files
    + __example__: the example given in the description
    + __input__: the input generated for me
* __src__: source files
    + __main.rs__: argument parsing
    + __day_01.rs__: problem logic
* __Cargo.toml__: description of how to build this and its dependencies
