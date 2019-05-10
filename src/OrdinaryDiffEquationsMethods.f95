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

      do while(option /= 5) ! not equal
        print*, "Select one option [1: Euler | 2: Modified Euler | 3: Runge Kutta 3rd order | 4: Runge Kutta 4rd order  | 5: Exit] "
        read*, option

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
        case default
          print *, "The number you chose is not an option"
        end select
      end do

    end subroutine OrdinaryDifferentialEquations

end module OrdinaryDiffEquationsMethods
