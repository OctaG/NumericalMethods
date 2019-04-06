PROGRAM MetodosNumericos2

  	use LagrangeMethod
	use PowerSeriesMethod

  integer:: option
  option=0
  print*, "Welcome to this very cool program"
  print*, "Here you can choose between 4 amazing numerical methods to find the root of a non-linear function"
!10
  do while(option /= 5)
    print*, "Select one option [1: Lagrange | 2: Power Series| 5: Salir] "
    read*, option

    select case(option)
    case (1)
      print*, "Lagrange Metodo"
      call Lagrange()
    case(2)
    	print*, "Power Series"
        call PowerSeries()
    case default
      print *, "The number you chose is not an option"
    end select
  end do

END PROGRAM MetodosNumericos2

