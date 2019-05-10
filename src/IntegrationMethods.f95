module IntegrationMethods

    !Modules used
    use IntegrationMethod
  
    contains
  
      !This is the menu for all the root finding methods
      subroutine Integration()
  
        integer:: option
  
        print*, "Here you can choose between 4 amazing numerical methods create functions from data"
  
        do while(option /= 5) ! not equal
          print*, "Select one option [1: Simpson1/3 | 2: Simpson3/8 | 3: Trapezoidal| 4: Exit] "
          read*, option
  
          select case(option)
          case (1)
            print*, "Simpson1/3"
            call Simpson13()
          case (2)
            print *, "Simpson3/8"
            call Simpson38()
          case (3)
            print *, "Trapezoidal"
            call Trapezoidal()
          case (4)
            print*, "You have left the Integration module"
          case default
            print *, "The number you chose is not an option"
          end select
        end do
  
      end subroutine Integration
  
  end module IntegrationMethods
  