# Advent of OCaml

Advent of OCaml is a collection of my personal answers to the Advent of Code
puzzles implemented in the OCaml programming language. This repository showcases
my solutions to each day's puzzle, providing insights into how I approached and
solved the problems.

## Table of Contents

- [Introduction to Advent of Code](#introduction-to-advent-of-code)
- [Getting Started](#getting-started)
- [Project Structure](#project-structure)
- [Running Solutions](#running-solutions)
- [Contributing](#contributing)
- [Resources](#resources)
- [License](#license)

## Introduction to Advent of Code

Advent of Code is an annual event that takes place during the month of December.
Each day, starting from December 1st, a new programming puzzle is released.
These puzzles are designed to be fun and challenging, allowing participants to
learn and improve their problem-solving skills.

The puzzles cover a wide range of topics and algorithms, making it a great
opportunity to explore different programming languages. In this project, I
focused on solving the Advent of Code puzzles using OCaml.

## Getting Started

To get started with Advent of OCaml, follow these steps:

1. **Install OCaml**: Ensure that OCaml is installed on your system. You can
   download and install it from the official website:
   [https://ocaml.org](https://ocaml.org).

2. **Clone the Repository**: Clone the Advent of OCaml repository to your local
   machine using the following command:

   ```
   git clone https://github.com/chrsmutti/advent_of_ocaml.git
   ```

3. **Install Dependencies**: Navigate to the project directory and install the
   required dependencies using the following command:

   ```
   opam install dune base stdio
   ```

## Project Structure

```
advent_of_ocaml/
├── bin/
│   ├── day01.ml
│   ├── day02.ml
│   └── ...
├── lib/
│   └── advent.ml
└── data/
    ├── day01.txt
    ├── day02.txt
    └── ...
```

- The `bin` directory contains OCaml files (`day01.ml`, `day02.ml`, etc.) that
  contain my individual solutions to each day's puzzle. Each file represents the
  solution for a specific day.

- The `lib` directory contains the `advent.ml` file, which can be used to store
  common utility functions or modules that are shared across different puzzles.

- The `data` directory stores the puzzle input files (`day01.txt`, `day02.txt`,
  etc.) required for each day's puzzle. These files contain the input data that
  needs to be processed by the corresponding solution file in the `bin`
  directory.

## Running Solutions

To run the solutions for each day's puzzle in the Advent of OCaml project,
follow these steps:

1. Ensure that you have set up the project and have the required dependencies
   installed. If not, refer to the "Getting Started" section in the README for
   instructions.

2. Open a terminal and navigate to the root directory of the project.

3. Build the project using Dune by running the following command:

   ```
   dune build
   ```

4. To run the solution for a specific day, execute the corresponding executable
   using the `dune exec` command. The executable names follow the pattern
   `dayXX.exe`, where `XX` represents the day number. For example, to run Day
   1's solution, use the following command:

   ```
   dune exec bin/day01.exe
   ```

   This will execute the solution and display the result based on the provided
   input file located in the `data` directory.

5. Repeat the above step for each day's puzzle by replacing `XX` with the
   respective day number.

Make sure that the input data file (`dayXX.txt`) for each day's puzzle is
present in the `data` directory before running the solution.

By following these steps, you can run the solutions for each day's puzzle and
see the corresponding output.

## Contributing

If you would like to contribute to Advent of OCaml, you're welcome to do so!
Here are a few guidelines to follow:

- Fork the repository and create a new branch for your changes.

- Make your modifications, ensuring that the code follows the existing style and
  conventions.

- Commit your changes and push them to your forked repository.

- Submit a pull request, describing the changes you have made and the rationale
  behind them.

Your contributions will be reviewed, and if they align with the project's goals
and quality standards, they will be merged into the main repository.

## Resources

To learn more about OCaml and its syntax, you can refer to the following
resources:

- Official OCaml Website: [https://ocaml.org](https://ocaml.org)
- Real World OCaml:
  [https://dev.realworldocaml.org](https://dev.realworldocaml.org)
- OCaml Documentation: [https://ocaml.org/docs](https://ocaml.org/docs)

Additionally, the Advent of Code website itself provides a great opportunity to
explore different algorithms and problem-solving techniques.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file
for more information.
