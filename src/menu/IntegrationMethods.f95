module IntegrationMethods

    !Modules used
    use IntegrationMethod
  
    contains
  
      !This is the menu for all the root finding methods
      subroutine Integration()
  
        integer:: option
  
        print*, "Here you can choose between 4 amazing numerical methods create functions from data"
        option = -1
        do while(option /= 5) ! not equal
          print *, "=== Numerical Integration ==="
          call funcionIntegralHumanize()
          print*,""
          print*, "Select one option:"
          print*, " [ 1: Simpson1/3 ]"
          print*, " [ 2: Simpson3/8 ]"
          print*, " [ 3: Trapezoidal with Function ]"
          print*, " [ 4: Trapezoidal with Data ] "
          print*, " [ 5: Exit ]"
          read*, option
          call system('clear')
          select case(option)
          case (1)
            print*, "Simpson1/3"
            call printReminder()
            call Simpson13()
          case (2)
            print *, "Simpson3/8"
            call printReminder()
            call Simpson38()
          case (3)
            print *, "Trapezoidal with Function"
            call printReminder()
            call Trapezoidal()
          case (4)
            print *, "Trapezoidal with Data"
            print*, "-- Remeber that the points must be in Points.txt under the appropiate format."
            print*, "-- Check documentation if needed."
            print*,""
            call TrapezoidalWithData()
          case (5)
            print*, "You have left the Integration module"
            print*,""
          case default
            print *, "The number you chose is not an option"
            print*,""
          end select
        end do
  
      end subroutine Integration
      subroutine printReminder()
        print*, "-- Remeber that the points must be in function.f95 under the appropiate format."
        print*, "-- Check documentation if needed."
        print*,""
      end subroutine printReminder
  end module IntegrationMethods
  