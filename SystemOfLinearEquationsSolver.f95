module SystemOfLinearEquationsSolver

  !Modules used
  use GaussianEliminationMethod
  use LU_DecompositionMethod

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
          call GaussianElimination()
        case(2)
        	print*, "LU Decomposition"
          call LU_Decomposition()
        case(3)
          print*, "Gauss-Seidel"
          !Aqui va la llamada a GaussSeidel()
        case(4)
          print*, "You have left the system of linear equations solver module"
        case default
          print *, "The number you chose is not an option"
        end select

      end do
    end subroutine SystemOfLinearEquations

end module SystemOfLinearEquationsSolver
