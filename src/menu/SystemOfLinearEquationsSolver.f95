module SystemOfLinearEquationsSolver

  !Modules used
  use GaussianEliminationMethod
  use LU_DecompositionMethod
  use GaussSeidelMethod

  contains

    !This is the menu for all the systems of linear equation methods
    subroutine SystemOfLinearEquations()

      integer :: option
      option=0

      print*, "Here you can choose between 3 amazing numerical methods to solve systems of linear equations"

      do while(option /= 4)
        print*, "=== Systems Of Linear Equations ==="
        print*,""
        print*, "Select one option"
        print*, " [ 1: Gaussian Elimination ]"
        print*, " [ 2: LU Decomposition ]"
        print*, " [ 3: Gauss-Seidel ]"
        print*, " [ 4: Go back ] "
        read*, option
        call system('clear')
        select case(option)
        case (1)
          print*, "Gaussian Elimination"
          call printReminder()
          call GaussianElimination()
        case(2)
        	print*, "LU Decomposition"
          print*, "-- If you want to use another Right Hand Side the data must be in RHS.txt under the appropiate format"
          call printReminder()
          call LU_Decomposition()
        case(3)
          print*, "Gauss-Seidel"
          call printReminder()
          print*, "Remember that the matrix must already be in heavy diagonal form"
          call GaussSeidel()
        case(4)
          print*, "You have left the system of linear equations solver module"
          print*,""
        case default
          print *, "The number you chose is not an option"
          print*,""
        end select

      end do
    end subroutine SystemOfLinearEquations

    subroutine printReminder()
      print*, "-- Remember that the matrix data is in the default file myData.txt under the appropiate format."
      print*, "-- Check documentation if needed."
      print*,""
    end subroutine printReminder

end module SystemOfLinearEquationsSolver
