PROGRAM MetodosNumericos

  use BisectionMethod
  use FalsePositionMethod
  use NewtonMethod
  use SecanteMethod

  integer:: option

  print*, "Welcome to this very cool program"
  print*, "Here you can choose between 4 amazing numerical methods to find the root of a non-linear function"

  do while(option /= 5)
    print*, "Select one option [1: Bisection | 2: False- Position | 3: Newton-Raphson | 4: Secant | 5: Salir] "
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
      print*, "Good Bye"
      print*, "A happy face before you leave: =)"
    case default
      print *, "The number you chose is not an option"
    end select
  end do

END PROGRAM MetodosNumericos
