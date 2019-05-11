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
  
        print*, "Here you can choose between 4 amazing numerical methods create functions from data"
  
        do while(option /= 5) ! not equal
          print*, "Select one option [1: Linear Regression | 2: Polynomial Regression |"
          print *, "3: Exponential Regression | 4: Logarithmic Regression | 5: Exit] "
          read*, option
  
          select case(option)
          case (1)
            print*, "Linear"
            call Linear()
          case (2)
            print *, "Polynomial"
            call PRegression()
          case (3)
            print *, "Exponential"
            call LinearEx()
          case (4)
            print *, "Logarithmic"
            call LinearLog()
          case (5)
            print*, "You have left the regression module"
          case default
            print *, "The number you chose is not an option"
          end select
        end do
  
      end subroutine Regression
  
  end module RegressionMethods
  