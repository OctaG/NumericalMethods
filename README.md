---
title: Numerical Methods in Fortran
fontsize: 12pt
---
\tableofcontents
\newpage

# Implemented Methods

In this proyect you can find:

1. **Solution of Non Linear Equations**
    a. Bisection Method
    b. False Position Method
    c. Newton Raphson Method
    d. Secant Method
2. **Solution of Systems of Linear Equations**
    a. Gaussian Elimination
    b. LU Decomposition
    c. Gauss-Seidel
3. **Interpolation**
    a. Power Series Method
    b. Lagrange
    c. Newton Divided Differences
4. **Regression**
    a. Polynomial Regression (including linear regression)
    b. Exponential Regression
    c. Logarithmic Regression
5. **Numerical Integration**
    a. Trapezoidal Rule
    b. Simpson 1/3
    c. Simpson 3/8
6. **Solution of Ordinary Differential**
    a. Euler Method
    b. Modified Euler
    c. Runge Kutta 3rd order
    d. Runge Kutta 4th order

\newpage

# How to Set Up

**To compile:**

1. Change the current working directory to the project's location :
    a. `$ cd NumericalMethods`
2. Then you have two options to compile:
    a. `$ make all`
    b. `$ make`

*note: if you change the functions in `functions.f95` you have to run the command to compile again*

**To run the executable file:**

1. Change the current working directory to the project's location and after compile it
    a. `$ ./methods.out`

**To clean binary files**

a. `$ make clean`

# How to Use
Inputs files are in directory `inputs/`

*Note: For `txt` files the numbers must be separated by spaces*

\newpage


## Solution of Non Linear Equations

**Methods Included:** Bisection Method, False Position Method, Newton Raphson Method, Secant Method

**Required file:** `functions.f95`

**Format:** The equation must be written in frontan sintax.
It is needed:

- The function $f(x)$
- The derivate of the function $\frac{d}{dx}f(x)$ (only if you want to use Newton Raphson Method)
- A visualization (print) of the current ecuation (Optional)

**Aditional input:** Two points $[a,b]$ that enclose the root of the function. The relative tolerance error $t$ in percentage $(0 < t < 1)$ and the maximum number of iterations $n$.

**Output:** Root found $x$ after reach maximun number of iterations $n$ or the tolerance $t$.

*Note: recall that you have to compile again if you change something*

\newpage

## System Of Linear Equations

**Methods Included:** Gauss Elimination, LU Decomposition and Gauss-Seidel

**Required file:** `myData.txt`

**Format:**

The appropiate format of the data is as follows:

$n$

$a_{1,1}\:a_{1,2}\;...\:a_{1,n}$

$a_{2,1}\:a_{2,2}\:...\:a_{2,n}$

.

.

.

$a_{n,1}\:a_{n,2}\:...\:a_{n,n}$

$b_1\:b_2\:...\:b_{n}$

Where $\boldsymbol{n}$ is the number of columns and rows of matrix $\boldsymbol{[A]}$, followed by $\boldsymbol{a_i}$ numbers in the matrix and finally $\boldsymbol{b_i}$ numbers of the matrix $\boldsymbol{[B]}$.

*note: For Gauss-Seidel the matrix must be in heavy diagonal form*

**Aditional input:** None

**Output:** The resulting matrix $[A]$ will appear first, followed by the results of $X_1$ to $X_n$

\newpage

## Interpolation

**Methods Included:** Power Series Method, Lagrange, Newton Divided Differences

**Required file:** `Points.txt`

**Format:**

The appropiate format of the data is as follows:
$n$
$x_1,\:x_2,\:x_3,\:...,\:x_n$
$y_1,\:y_2,\:y_3,\:...,\:y_n$

Where $\boldsymbol{n}$ is the number of points, followed by the pairs of numbers that represents the points.

**Aditional input:**  The value of $x$ that they want to use, then they will be asked for the order of the polynomial and, finally,
the user will be asked for the point from which they want to start the evaluation.

**Output:** R

\newpage

## Sample

**Methods Included:** something

**Required file:** `Points.txt`

**Format:**

**Aditional input:** None

**Output:** Something

\newpage