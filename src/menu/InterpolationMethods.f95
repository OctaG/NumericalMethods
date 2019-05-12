module InterpolationMethods

  !Modules used
  use PowerSeriesMethod
  use LagrangeMethod
  use NewtonDivededDifference

  contains

    !This is the menu for all the systems of interpolation  methods
    subroutine Interpolation()

      integer :: option
      option=0

      print*, "Here you can choose between 3 amazing numerical methods to interpolate data"

      do while(option /= 4)
        print *, "=== Interpolation ==="
        print*,""
        print*, "Select one option"
        print*, " [ 1: Power Series ]"
        print*, " [ 2: Lagrange ]"
        print*, " [ 3: Newton-Divided Differences ]"
        print*, " [ 4: Back to menu ] "
        read*, option
        call system('clear')
        select case(option)
        case (1)
          print*, "Power Series"
          call printReminder()
          call PowerSeries()
        case(2)
          print*, "Lagrange"
          call printReminder()
          call Lagrange()
        case(3)
          print*, "Newton-Divided Differences"
          call printReminder()
          call NewtonDivided()
        case(4)
          print*, "You have left the interpolation module"
          print*,""
        case default
          print *, "The number you chose is not an option"
          print*,""
        end select

      end do
    end subroutine Interpolation

    subroutine printReminder()
      print*, "-- Remeber that the points must be in Points.txt under the appropiate format."
      print*, "-- Check documentation if needed."
      print*,""
    end subroutine printReminder
end module InterpolationMethods
