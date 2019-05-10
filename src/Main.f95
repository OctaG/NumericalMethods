PROGRAM Main

  !Modules used
  use RootFindingMethods
  use SystemOfLinearEquationsSolver
  use InterpolationMethods
  use RegressionMethods
  use IntegrationMethods
  use OrdinaryDiffEquationsMethods

  integer:: option

  print*, "Welcome to this very cool program"

  !This is the menu for all the different methods
  do while(option /= 7) ! not equal

    print*, "MAIN MENU"
    print*, "Select one option [ 1: Root Finding ] |"
    print*, "| [2: System Of Linear Equations Solver] | "
    print*, "| [3: Interpolation] |"
    print*, "| [4: Regression] |"
    print*, "| [5: Integration] |" 
    print*, "| [6: Ordinary Differential Equations] |"
    print*, "| [7: Exit] |"
    read*, option

    select case(option)
    case (1)
      print*, "Root Finding Methods"
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
    case (7)
      print*, "Good Bye"
      print*, "A happy face before you leave: =)"
    case default
      print *, "The number you chose is not an option"
    end select
  end do

END PROGRAM Main
