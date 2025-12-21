# Advent of Code 2025
Here are my solutions for [2025 Advent of Code](https://adventofcode.com/2025).

## Problem Notes

### Day 1 Part 1
Implementation.

### Day 1 Part 2
There are some tricky edge cases to take note of &mdash; for example, when the current dial state is at `0` or `1`, and the rotation is `L1`.

### Day 2
Brute force, which the list monad makes very convenient to represent.

### Day 3 Part 2
Bottom up dynamic programming. Iterate on each line from the right, keeping track of the maximum number found for each digit count of number.

### Day 4
Implementation, using Haskell arrays to represent the grid.

### Day 5 Part 1
Check all ID ranges for each ID.

### Day 5 Part 2
Sort ranges by start and end, then merge adjacent ranges that overlap.

### Day 6 Part 1
Parsing.

### Day 6 Part 2
Convenient to parse using `transpose`, then splitting on columns of all whitespace.

### Day 7
Iterate through the lines while keeping track of the beams.
For part 2, also keep track of the number of paths a certain beam could come from.

### Day 8
Partial application of [Kruskal's algorithm](https://en.wikipedia.org/wiki/Kruskal%27s_algorithm).
Use the [disjoint set](https://en.wikipedia.org/wiki/Disjoint-set_data_structure) data structure.

### Day 9 Part 1
Brute force every rectangle.

### Day 9 Part 2
Draw out the polygon in a grid, then flood fill from the edges to determine the inside of the polygon.
Use a 2D prefix sum array of the interior of the polygon to check if any rectangle is within the polygon.
Use coordinate compression on the tile coordinates to shrink the grid and make the flood fill viable.
Finally, brute force every rectangle, checking if it is inside the polygon.

### Day 10 Part 1
Buttons need only be clicked at most once, so it suffices to brute force every possible button press combination.

### Day 10 Part 2
This is a kind of [integer linear programming](https://en.wikipedia.org/wiki/Integer_programming).
It's possible to solve this using a linear algebra based solution.

The solution in this repository is a aort of "halving parity-checking" solution.
Check all button press combinations, where each button is pressed at most once, and ensure the leftover required joltages are all even.
Then, divide the required joltages by two and recurse.

### Day 11
Standard [directed acyclic graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph) problem.
For each node, find its predecessors, then do top-down memoized dynamic programming to count paths.
For part 2, modify the previous solution to check paths `svr -> dac -> fft -> out` and `svr -> fft -> dac -> out`.

### Day 12
This is an NP-hard [packing problem](https://en.wikipedia.org/wiki/Packing_problems).

It turns out that the data is weak enough to solve with heuristics only, namely:

- Pretend each shape is a 3x3 square, and if it is possible to pack the required 3x3 squares, then it is possible to pack the shapes
- Count the total number of `#` tiles of all required shapes, and if there is not enough area in the rectangle, then it is not possible to pack the shapes
