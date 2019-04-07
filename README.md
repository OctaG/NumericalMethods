# Numerical Methods
Fortran project with the implementation of different numerical methods

## Set up

**To compile:**
1. Change the current working directory to the project's location :

   a. `$ cd NumericalMethods`

2. Then you have two options to compile:

   a. `$ make all`

   b. `$ make`

*note: if you change the functions in `functions.f95` you have to run the command to compile again*

**To run the executable file:**
1. Change the current working directory to the project's location and after compile it
2. `$ ./methods.out`

**To clean binary files**
a. `$ make clean`

## Data inputs
Inputs files are in directory `inputs`

### System Of Linear Equations

####What do Gauss Elimination, LU Decomposition and Gauss-Seidel need to work?

The only thing that the three programs need to work is correctly is that `myData.txt` contains the data of the
system of equations in a matrix format. You must ensure that it is written in the correct format.

####How can I update the data of the system of equations I want to solve? What is the appropiate format?
1. Open `myData.txt`
2. The appropiate format of the data is as follows:
-  $n$ (This is the number of colums and rows of matrix $[A]$)
-  $a_{1,1}\:a_{1,2}\;...\:a_{1,n}$

   $a_{2,1}\:a_{2,2}\:...\:a_{2,n}$
   .

   .

   .

   $a_{n,1}\:a_{n,2}\:...\:a_{n,n}$

-  $b1\:b2\:...\:b_{n}$
3. The numbers must be separated by spaces

#### Where can I find my the results of my system of equations?
The results will be saved in `results.txt`
The resulting matrix $[A]$ will appear first, followed by the results of $X_1$ to $X_n$

### Interpolation

**How to use Power Series and Lagrange**
1. Both programs read the points from a text file called `Points.txt`
2. The appropiate format of the data is as follows:
-  $n$ (This is the number of points)
-  $x_1,\:x_2,\:x_3,\:...,\:x_n$
-  $y_1,\:y_2,\:y_3,\:...,\:y_n$

3. The numbers must be separated by spaces.
4. Both programs: the user will be asked for the value of $x$ that they want to use, then they will be asked for the order of the polynomial and, finally,
the user will be asked for the point from which they want to start the evaluation.

#### How do the Power Series and Lagrange programs work?
1. The LagrangeMethod program works like a function and it can work without the need of other programs. It only needs the `Points.txt` text file. It writes the final results.
in the `LagrangeOutcome.txt`
2. The PowerSeriesMethod program needs the GaussforPower program in order to get the solution of the matrix that it creates. After the GaussforPower program is done solving the matrix of the PowerSeriesMethod program (the matrix is saved in `PowerSeriesMatrix.txt` and the solution of the matrix in `ResultsPowerSeries.txt`), the Power Series program reads the solution and writes the final result in the `PowerSeriesFinalOutcome.txt`
