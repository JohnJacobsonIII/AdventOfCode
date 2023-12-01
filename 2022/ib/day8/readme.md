# Attempt at Day 8

[Description](https://adventofcode.com/2022/day/8)


## Repeated here

--- __Day 8: Treetop Tree House__ ---

The expedition comes across a peculiar patch of tall trees all planted carefully in a grid. The Elves explain that a previous expedition planted these trees as a reforestation effort. Now, they're curious if this would be a good location for a [tree house](https://en.wikipedia.org/wiki/Tree_house).

First, determine whether there is enough tree cover here to keep a tree house __hidden__. To do this, you need to count the number of trees that are __visible from outside the grid__ when looking directly along a row or column.

The Elves have already launched a [quadcopter](https://en.wikipedia.org/wiki/Quadcopter) to generate a map with the height of each tree (your puzzle input). For example:

```
30373
25512
65332
33549
35390
```

Each tree is represented as a single digit whose value is its height, where 0 is the shortest and 9 is the tallest.

A tree is __visible__ if all of the other trees between it and an edge of the grid are __shorter__ than it. Only consider trees in the same row or column; that is, only look up, down, left, or right from any given tree.

All of the trees around the edge of the grid are __visible__ - since they are already on the edge, there are no trees to block the view. In this example, that only leaves the __interior nine trees__ to consider:

- The top-left 5 is __visible__ from the left and top. (It isn't visible from the right or bottom since other trees of height 5 are in the way.)
- The top-middle 5 is __visible__ from the top and right.
- The top-right 1 is not __visible__ from any direction; for it to be visible, there would need to only be trees of height __0__ between it and an edge.
- The left-middle 5 is __visible__, but only from the right.
- The center 3 is not __visible__ from any direction; for it to be visible, there would need to be only trees of at most height 2 between it and an edge.
- The right-middle 3 is __visible__ from the right.
- In the bottom row, the middle 5 is __visible__, but the 3 and 4 are not.

With 16 trees visible on the edge and another 5 visible in the interior, a total of __21__ trees are visible in this arrangement.

Consider your map; __how many trees are visible from outside the grid?__

Your puzzle answer was 1798.

--- __Part Two__ ---

Content with the amount of tree cover available, the Elves just need to know the best spot to build their tree house: they would like to be able to see a lot of __trees__.

To measure the viewing distance from a given tree, look up, down, left, and right from that tree; stop if you reach an edge or at the first tree that is the same height or taller than the tree under consideration. (If a tree is right on the edge, at least one of its viewing distances will be zero.)

The Elves don't care about distant trees taller than those found by the rules above; the proposed tree house has large [eaves](https://en.wikipedia.org/wiki/Eaves) to keep it dry, so they wouldn't be able to see higher than the tree house anyway.

In the example above, consider the middle 5 in the second row:

```
30373
25512
65332
33549
35390
```
- Looking up, its view is not blocked; it can see __1__ tree (of height 3).
- Looking left, its view is blocked immediately; it can see only __1__ tree (of height 5, right next to it).
- Looking right, its view is not blocked; it can see __2__ trees.
- Looking down, its view is blocked eventually; it can see __2__ trees (one of height 3, then the tree of height 5 that blocks its view).

A tree's __scenic score__ is found by __multiplying together__ its viewing distance in each of the four directions. For this tree, this is __4__ (found by multiplying 1 * 1 * 2 * 2).

However, you can do even better: consider the tree of height 5 in the middle of the fourth row:

```
30373
25512
65332
33549
35390
```

- Looking up, its view is blocked at __2__ trees (by another tree with a height of 5).
- Looking left, its view is not blocked; it can see __2__ trees.
- Looking down, its view is also not blocked; it can see __1__ tree.
- Looking right, its view is blocked at __2__ trees (by a massive tree of height 9).

This tree's scenic score is __8__ (2 * 2 * 1 * 2); this is the ideal spot for the tree house.

Consider each tree on your map. __What is the highest scenic score possible for any tree?__

Your puzzle answer was 259308.


# File manifest:

* __data__: input files
    + __example__: the example given in the description
    + __input__: the input generated for me
* __src__: source files
    + __main.rs__: argument parsing
    + __day8.rs__: problem logic
* __Cargo.toml__: description of how to build this and its dependencies