module InterpolationMethods

  !Modules used
  use PowerSeriesMethod
  use LagrangeMethod

  contains

    !This is the menu for all the systems of interpolation  methods
    subroutine Interpolation()

      integer :: option
      option=0

      print*, "Here you can choose between 3 amazing numerical methods to interpolate data"

      do while(option /= 4)
        print*, "Select one option [1: Power Series | 2: Lagrange | 3: Newton-Divided Differences | 4: Back to menu] "
        read*, option

        select case(option)
        case (1)
          print*, "Power Series"
          call PowerSeries()
        case(2)
        	print*, "Lagrange"
          call Lagrange()
        case(3)
          print*, "Newton-Divided Differences"
          !Aqui va la llamada a NewtonDivided()
        case(4)
          print*, "You have left the interpolation module"
        case default
          print *, "The number you chose is not an option"
        end select

      end do
    end subroutine Interpolation

end module InterpolationMethods
