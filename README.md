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

*Note: if you change the functions in `functions.f95` you have to run the command to compile again*

**To run the executable file:**

1. Change the current working directory to the project's location and after compile it
    a. `$ ./methods.out`

**To clean binary files**

a. `$ make clean`

# How to Use
Questions or request [GitHub Repo](https://github.com/OctaG/NumericalMethods "Link to repo")


- Inputs files are in directory `inputs/`
- Outputs files are in directory `results/`

*Note: For `txt` files in* **inputs** *the numbers must be separated by spaces*

*Note 2: For `txt` files in* **outputs** *if the file already exists, the info will append to the current text in the file*


\newpage


## Solution of Non Linear Equations

**Methods Included:**

- Bisection Method
- False Position Method
- Newton Raphson Method
- Secant Method

**Required file:** `functions.f95`

**Format:** The equation must be written in frontan sintax.
It is needed for:

```
FUNCTION funcion(x)
FUNCTION funcionDerivada(x)
SUBROUTINE funcionHumanize()
```

- The function $f(x)$
- The derivate of the function $\frac{d}{dx}f(x)$ (only if you want to use Newton Raphson Method or Secant Method)
- A visualization (print) of the current ecuation (Optional)



**Aditional input:** 

- *Bisection Method and False Position Method*: Two points $[a,b]$ that enclose the root of the function.
- *Newton Raphson Method and Secant Method*: An initial $x_1$, it could be an educated guess
- All methods: The relative tolerance error $t$ in percentage $(0 < t < 1)$ and the maximum number of iterations $n$.

**Output:**

- Root found $x$ after reach maximun number of iterations $n$ or the tolerance $t$.
- *Note: recall that you have to compile again if you change something*

\newpage

## System Of Linear Equations

**Methods Included:** 

- Gauss Elimination
- LU Decomposition
- Gauss-Seidel

**Required file:** `myData.txt`

**Format:**

- The appropiate format of the data is as follows:

$n$

$a_{1,1}\:a_{1,2}\;...\:a_{1,n}$

$a_{2,1}\:a_{2,2}\:...\:a_{2,n}$

.

.

.

$a_{n,1}\:a_{n,2}\:...\:a_{n,n}$

$b_1\:b_2\:...\:b_{n}$

- Where $\boldsymbol{n}$ is the number of columns and rows of matrix $\boldsymbol{[A]}$, followed by $\boldsymbol{a_i}$ numbers in the matrix and finally $\boldsymbol{b_i}$ numbers of the matrix $\boldsymbol{[B]}$.
- *Note: For Gauss-Seidel the matrix must be in heavy diagonal form*

**Aditional input:** None

**Output:**

- The resulting matrix $[A]$ will appear first, followed by the results of $X_1$ to $X_n$

\newpage

## Interpolation

**Methods Included:**

- Power Series Method
- Lagrange
- Newton Divided Differences

**Required file:** `Points.txt`

**Format:**

- The appropiate format of the data is as follows:


$n$

$x_1,\:x_2,\:x_3,\:...,\:x_n$

$y_1,\:y_2,\:y_3,\:...,\:y_n$


- Where $\boldsymbol{n}$ is the number of points, followed by the pairs of numbers that represents the points.

**Aditional input:** 

- The order of the polynomial $degree$
- *Power Series Method and Lagrange*: The user will be asked for the point from which they want to start the evaluation.
- The value of $x$ that they want to calculate

**Output:**

- *Newton Divided Differences* : All the coefficients of the possible polynomials and the evaluation of the points that the user would have calculated.

\newpage


## Regression

**Methods Included:** 

- Linear Regression
- Polynomial Regression 
- Exponential Regression
- Logarithmic Regression

**Required file:** 

- Linear Regression : `Points2.txt`
- Polynomial Regression : `Points2.txt`
- Exponential Regression : `Points2.txt`
- Logarithmic Regression: `Points3.txt`

**Format:** For both files the appropiate format of the data is as follows:

$n$

$x_1,\:x_2,\:x_3,\:...,\:x_n$

$y_1,\:y_2,\:y_3,\:...,\:y_n$


- Where $\boldsymbol{n}$ is the number of points, followed by the pairs of numbers that represents the points.

**Aditional input:** 

- Number of points you want to use
- Point from which you want to start
- $m$ points to evaluate (optional)

**Output:** 

- $S_t$, $S_r$, $r^2$, $r$
- Equation obtained $f(x)$
- Results for every input given

\newpage


## Integration

**Methods Included:** 

- Trapezoidal Rule
- Simpson $\frac{1}{3}$
- Simpson $\frac{3}{8}$

**Required file:** 

- Trapezoidal Rule (with data): `Points.txt`
- Trapezoidal Rule (with function) and Simpson: `functions.f95`

**Format:**

- About `Points.txt`

$n$

$x_1,\:x_2,\:x_3,\:...,\:x_n$

$y_1,\:y_2,\:y_3,\:...,\:y_n$

- Where $\boldsymbol{n}$ is the number of points, followed by the pairs of numbers that represents the points.

- For `functions.f95`, the equation must be written in frontan sintax. It is needed

```
FUNCTION funcionIntegral(x)
SUBROUTINE funcionIntegralHumanize()
```

- The function $f(x)$ that you want to integrate.
- An easy way to read the $f(x)$ (optional)

**Aditional input:** 

- *Trapezoidal Rule (with data)*: None
- *Trapezoidal Rule (with function), Simpson $\frac{1}{3}$ and Simpson $\frac{3}{8}$* : It its needed lower limit $a$, upper limit $b$, tolerance $t$ in percentage $0 < t < 1$, and $n$ limit of iterations.

**Output:** The aproximated value of $\int_{a}^{b} f(x) dx$

*Note: recall that you have to compile again if you change something*

\newpage

## Solution of Ordinary Differential

**Methods Included:** 

- Euler Method
- Modified Euler
- Runge Kutta $3^{rd}$ order
- Runge Kutta $4^{th}$ order

**Required file:** `functions.f95`

**Format:** The equation must be written in frontan sintax. It is needed

```
FUNCTION fdexy(x,y)
SUBROUTINE funcionDiffHumanize()
```
- The function $f(x,y)$ that you want to integrate.
- An easy way to read the $f(x,y)$ (optional)

**Aditional input:** 

- Number of intervals
- Tolerance $(0 < t < 1)$
- $a$, $b$ 
- $f(a)$
- Fixed
- Maximun number of iterations

**Output:** Pair of points $(x,y)$.

*Note: recall that you have to compile again if you change something*
