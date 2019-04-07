module RootFindingMethods

  !Modules used
  use BisectionMethod
  use FalsePositionMethod
  use NewtonMethod
  use SecanteMethod

  contains

    !This is the menu for all the root finding methods
    subroutine RootFinding()

      integer:: option

      print*, "Here you can choose between 4 amazing numerical methods to find the root of a non-linear function"

      do while(option /= 5) ! not equal
        call funcionHumanize()
        print*, "Select one option [1: Bisection | 2: False- Position | 3: Newton-Raphson | 4: Secant | 5: Exit] "
        read*, option

        select case(option)
        case (1)
          print*, "Bisection"
          call Bisection()
        case (2)
          print *, "False-Position"
          call FalsePosition()
        case (3)
          print *, "Newton-Raphson"
          call Newton()
        case (4)
          print *, "Secant"
          call Secante()
        case (5)
          print*, "You have left the root finding module"
        case default
          print *, "The number you chose is not an option"
        end select
      end do

    end subroutine RootFinding

end module RootFindingMethods
