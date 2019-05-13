module RegressionMethods

    !Modules used
    use LinearMethod
    use PRMethod
    use LinearExponentialMethod
    use LinearLogMethod
  
    contains
  
      !This is the menu for all the root finding methods
      subroutine Regression()
  
        integer:: option
        option = 0
  
        print*, "Here you can choose between 4 amazing numerical methods create functions from data"
  
        do while(option /= 5) ! not equal
          print *, "=== Regression ==="
          print*,""
          print*, "Select one option:"
          print*, " [ 1: Linear Regression ]"
          print*, " [ 2: Polynomial Regression ]"
          print*, " [ 3: Exponential Regression ]"
          print*, " [ 4: Logarithmic Regression ]"
          print*, " [ 5: Exit ] "
          read*, option
          call system('clear')
          select case(option)
            case (1)
              print*, "Linear"
              call printReminder()
              call Linear()
            case (2)
              print *, "Polynomial"
              call printReminder()
              call PRegression()
            case (3)
              print *, "Exponential"
              call printReminder()
              call LinearEx()
            case (4)
              print *, "Logarithmic"
              call printReminder()
              call LinearLog()
            case (5)
              print*, "You have left the regression module"
              print*,""
            case default
              print *, "The number you chose is not an option"
              print*,""
          end select
        end do
      end subroutine Regression

      subroutine printReminder()
        print*, "-- Remeber that the default file is Points.txt, must be under the appropiate format."
        print*, "-- Check documentation if needed."
        print*,""
      end subroutine printReminder
  
  end module RegressionMethods
  