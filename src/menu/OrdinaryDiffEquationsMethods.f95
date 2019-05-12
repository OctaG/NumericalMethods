module OrdinaryDiffEquationsMethods

  !Modules used
  use EulerMethod
  use ModifiedEulerMethod
  use RungeKutta3Method
  use RungeKutta4Method

  contains

    !This is the menu for all the ord diff equations methods
    subroutine OrdinaryDifferentialEquations()

      integer:: option

      print*, "Here you can choose between 4 amazing numerical methods to find the solution of a differential equations"
      option = -1
      do while(option /= 5) ! not equal
        print*, "=== Root Finding Methods ==="
        call funcionDiffHumanize()
        print*,""
        print*, "Select one option"
        print*, " [ 1: Euler ]"
        print*, " [ 2: Modified Euler ]"
        print*, " [ 3: Runge Kutta 3rd order ]"
        print*, " [ 4: Runge Kutta 4rd order ]"
        print*, " [ 5: Exit] "
        read*, option
        call system('clear')
        select case(option)
        case (1)
          print*, "Euler"
          call Euler()
        case (2)
          print *, "Modified Euler"
          call ModifiedEuler()
        case (3)
          print *, "Runge Kutta 3rd order"
          call RungeKutta3()
        case (4)
          print *, "Runge Kutta 4rd order"
          call RungeKutta4()
        case (5)
          print*, "You have left the root ordinary differential equations module"
          print*,""
        case default
          print *, "The number you chose is not an option"
          print*,""
        end select
      end do

    end subroutine OrdinaryDifferentialEquations

end module OrdinaryDiffEquationsMethods
