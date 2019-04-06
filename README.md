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
