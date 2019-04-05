PROGRAM MetodosNumericos2

  use LagrangeMethod


  integer:: option
  option=0
  print*, "Welcome to this very cool program"
  print*, "Here you can choose between 4 amazing numerical methods to find the root of a non-linear function"

  do while(option /= 5)
    print*, "Select one option [1: Lagrange | 2: False- Position | 3: Newton-Raphson | 4: Secant | 5: Salir] "
    read*, option

    select case(option)
    case (1)
      print*, "Lagrange Metodo"
      call Lagrange()
    case default
      print *, "The number you chose is not an option"
    end select
  end do

END PROGRAM MetodosNumericos2

