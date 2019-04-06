# Numerical Methods
Fortran project with the implementation of different numerical methods

**To compile:**
1. Change the curring working directory to the project's location
2. `$ gfortran modulo_f.f95 BisectionMethod.f95 FalsePositionMethod.f95 NewtonMethod.f95 SecanteMethod.f95 GaussianEliminationMethod.f95 LU_DecompositionMethod.f95 PowerSeriesMethod.f95 LagrangeMethod.f95  RootFindingMethods.f95 SystemOfLinearEquationsSolver.f95 InterpolationMethods.f95  Main.f95`

**To run the executable file:**
1. Change the curring working directory to the project's location
2. `$ ./methods.out`

**To update the data of the systems of linear equations**
1. Open myData.txt
2. The appropiate format of the data is as follows:
-  n (This is the number of colums and rows of matrix [A])
-  a1,1 a1,2 ... a1,n
   a2,1 a2,2 ... a2,n
   .
   .
   .
   an,1 an,2 ... an,n
-  b1 b2 ... bn

**Where can I find my results?**
The results will be saved in results.txt
The resulting matrix [A] will appear first, followed by the results of X1 to Xn

**How to use Power Series and Lagrange**
1. Both programs read the points from a text file called Points.txt
2. The appropiate format of the data is as follows:
-  n (This is the number of points)
-  x1, x2, x3, ..., xn
-  y1, y2, y3, ..., yn

3. The numbers must be separated by spaces.
4. Both programs: the user will be asked for the value of x that they want to use, then they will be asked for the order of the polynomial and, finally, 
the user will be asked for the point from which they want to start the evaluation.

**How do the Power Series and Lagrange programs work**
1. The LagrangeMethod program works like a function and it can work without the need of other programs. It only needs the Points.txt text file. It writes the final results
in the LagrangeOutcome.txt
2. The PowerSeriesMethod program needs the GaussforPower prorgam in order to get the solution of the matrix that it creates. After the GaussforPower program is done
solving the matrix of the PowerSeriesMethod program (the matrix is saved in PowerSeriesMatrix.txt and the solution of the matrix in ResultsPowerSeries.txt), the
Power Series program reads the solution and writes the final result in the PowerSeriesFinalOutcome.txt