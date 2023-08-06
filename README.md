# Strassen Matrix Multiplication

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Description
This project contains an OCaml script for performing matrix multiplication using the Strassen algorithm.

The Strassen algorithm is a method for efficiently multiplying matrices. It employs a recursive divide-and-conquer approach to break down matrix multiplication into smaller subproblems, reducing the overall number of scalar multiplications.

## How to Use

To use this program, follow these steps:

1. Clone this repository to your local machine.
2. Navigate to the project directory.
3. Open the `Makefile` and take note of the available commands:
   - `make default`: Compiles the program.
   - `make run`: Runs the compiled program.
   - `make clean`: Cleans the project directory.
   - `make doc`: Generates OCaml documentation.
4. To specify the matrices to be multiplied, edit the following lines at the bottom of `strassen.ml`:
    ```ocaml
    let m1 = read_matrix_from_file "matrices/matrix1.txt";;
    let m2 = read_matrix_from_file "matrices/matrix1.txt";;
    ```

## License
The project is distributed under the MIT License, which allows you to use, modify, and distribute the tool freely, including for commercial purposes. 
However, you must provide appropriate credit to the [original author](https://github.com/nicolasCDT/).

## Author
This project was created by Nicolas Coudert. 
* Email: [nicolas@coudert.pro](mailto:nicolas@coudert.pro)
* GitHub: [nicolasCDT](https://github.com/nicolasCDT)

Feel free to report any issues or sggest improvements via GitHub issues.