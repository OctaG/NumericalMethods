PROGRAM Main

  !Modules used
  use RootFindingMethods
  use SystemOfLinearEquationsSolver
  use InterpolationMethods

  integer:: option

  print*, "Welcome to this very cool program"

  !This is the menu for all the different methods
  do while(option /= 4) ! not equal

    print*, "MAIN MENU"
    print*, "Select one option [1: Root Finding | 2: System Of Linear Equations Solver | 3: Interpolation | 4: Exit] "
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
      print*, "Good Bye"
      print*, "A happy face before you leave: =)"
    case default
      print *, "The number you chose is not an option"
    end select
  end do

END PROGRAM Main
