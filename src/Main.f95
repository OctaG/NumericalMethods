PROGRAM Main

    !Modules used
    use RootFindingMethods
    use SystemOfLinearEquationsSolver
    use InterpolationMethods
    use RegressionMethods
    use IntegrationMethods
    use OrdinaryDiffEquationsMethods

    integer:: option
    integer, parameter :: exit_op = 8

    call system('clear')
    print*, "Welcome to this very cool program!"
    print*, "It is strongly suggest to read the documentation (README.md)"
    print*,""

    !This is the menu for all the different methods
    do while(option /= exit_op) ! not equal
        print*, "MAIN MENU"
        print*, "Select one option:"
        print*, "| [ 1: Root Finding ] |"
        print*, "| [ 2: System Of Linear Equations Solver ] | "
        print*, "| [ 3: Interpolation ] |"
        print*, "| [ 4: Regression ] |"
        print*, "| [ 5: Integration ] |" 
        print*, "| [ 6: Ordinary Differential Equations ] |"
        print*, "| [ 7: Help! ] |"
        print*, "| [ 8: Exit ] |" !take care when modify this number
        read*, option
        call system('clear')
        select case(option)
            case (1)
                call system('clear')
                call RootFinding()
            case (2)
                print *, "System Of Linear Equations Solver"
                call SystemOfLinearEquations()
            case (3)
                print *, "Interpolation"
                call Interpolation()
            case (4)
                print *, "Regression"
                call Regression()
            case (5)
                print *, "Integration"
                call Integration()
            case (6)
                print*, "Ordinary Differential Equations"
                call OrdinaryDifferentialEquations()
            case(7)
                print*,"1. Solution of Non Linear Equations"
                print*,"-- a. Bisection Method"
                print*,"-- b. False Position Method"
                print*,"-- c. Newton Raphson Method"
                print*,"-- d. Secant Method"
                print*,"2. Solution of Systems of Linear Equations"
                print*,"-- a. Gaussian Elimination"
                print*,"-- b. LU Decomposition"
                print*,"-- c. Gauss-Seidel"
                print*,"3. Interpolation"
                print*,"-- a. Power Series Method"
                print*,"-- b. Lagrange"
                print*,"-- c. Newton Divided Differences"
                print*,"4. Regression"
                print*,"-- a. Polynomial Regression (including linear regression)"
                print*,"-- b. Exponential Regression"
                print*,"-- c. Logarithmic Regression"
                print*,"5. Numerical Integration"
                print*,"-- a. Trapezoidal Rule"
                print*,"-- b. Simpson 1/3"
                print*,"-- c. Simpson 3/8"
                print*,"6. Solution of Ordinary Differential"
                print*,"-- a. Euler Method"
                print*,"-- b. Modified Euler"
                print*,"-- c. Runge Kutta 3rd order"
                print*,"-- d. Runge Kutta 4th order"
            case (exit_op)
                print*, "Good Bye"
                print*, "A happy face before you leave: =)"
            case default
                print *, "The number you chose is not an option"
        end select
    end do

END PROGRAM Main
