# Attempt at Day 1

[Description](https://adventofcode.com/2022/day/1)


## Repeated here

--- __Day 1: Calorie Counting__ ---

Santa's reindeer typically eat regular reindeer food, but they need a lot of [magical energy](https://adventofcode.com/2018/day/25) to deliver presents on Christmas. For that, their favorite snack is a special type of __star__ fruit that only grows deep in the jungle. The Elves have brought you on their annual expedition to the grove where the fruit grows.

To supply enough magical energy, the expedition needs to retrieve a minimum of __fifty stars__ by December 25th. Although the Elves assure you that the grove has plenty of fruit, you decide to grab any fruit you see along the way, just in case.

Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants __one star__. Good luck!

The jungle must be too overgrown and difficult to navigate in vehicles or access from the air; the Elves' expedition traditionally goes on foot. As your boats approach land, the Elves begin taking inventory of their supplies. One important consideration is food - in particular, the number of __Calories__ each Elf is carrying (your puzzle input).

The Elves take turns writing down the number of Calories contained by the various meals, snacks, rations, etc. that they've brought with them, one item per line. Each Elf separates their own inventory from the previous Elf's inventory (if any) by a blank line.

For example, suppose the Elves finish writing their items' Calories and end up with the following list:

```
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
```

This list represents the Calories of the food carried by five Elves:

- The first Elf is carrying food with 1000, 2000, and 3000 Calories, a total of __6000__ Calories.
- The second Elf is carrying one food item with __4000__ Calories.
- The third Elf is carrying food with 5000 and 6000 Calories, a total of __11000__ Calories.
- The fourth Elf is carrying food with 7000, 8000, and 9000 Calories, a total of __24000__ Calories.
- The fifth Elf is carrying one food item with __10000__ Calories.

In case the Elves get hungry and need extra snacks, they need to know which Elf to ask: they'd like to know how many Calories are being carried by the Elf carrying the __most__ Calories. In the example above, this is __24000__ (carried by the fourth Elf).

Find the Elf carrying the most Calories. __How many total Calories is that Elf carrying?__

Your puzzle answer was 71471.

--- __Part Two__ ---

By the time you calculate the answer to the Elves' question, they've already realized that the Elf carrying the most Calories of food might eventually __run out of snacks__.

To avoid this unacceptable situation, the Elves would instead like to know the total Calories carried by the __top three__ Elves carrying the most Calories. That way, even if one of those Elves runs out of snacks, they still have two backups.

In the example above, the top three Elves are the fourth Elf (with 24000 Calories), then the third Elf (with 11000 Calories), then the fifth Elf (with 10000 Calories). The sum of the Calories carried by these three elves is __45000__.

Find the top three Elves carrying the most Calories. __How many Calories are those Elves carrying in total?__

Your puzzle answer was 211189.


# File manifest:

* __data__: input files
    + __example__: the example given in the description
    + __input__: the input generated for me
* __src__: source files
    + __main.rs__: argument parsing
    + __day1.rs__: problem logic
* __Cargo.toml__: description of how to build this and its dependencies