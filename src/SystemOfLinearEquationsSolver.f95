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
        print*, "Select one option [1: Gaussian Elimination | 2: LU Decomposition | 3: Gauss-Seidel | 4: Back to menu] "
        read*, option

        select case(option)
        case (1)
          print*, "Gaussian Elimination"
          call printReminder()
          call GaussianElimination()
        case(2)
        	print*, "LU Decomposition"
          call printReminder()
          call LU_Decomposition()
        case(3)
          print*, "Gauss-Seidel"
          call printReminder()
          print*, "Remember that the matrix must already be in heavy diagonal form"
          call GaussSeidel()
        case(4)
          print*, "You have left the system of linear equations solver module"
        case default
          print *, "The number you chose is not an option"
        end select

      end do
    end subroutine SystemOfLinearEquations

    subroutine printReminder()
      print*, "Remeber that the matrix data must be in myData.txt under the appropiate format. Check documentation if needed."
      print*, "The results of the system will be saved in results.txt"
    end subroutine printReminder

end module SystemOfLinearEquationsSolver
