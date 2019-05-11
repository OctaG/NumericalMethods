module RootFindingMethods

    !Modules used
    use BisectionMethod
    use FalsePositionMethod
    use NewtonMethod
    use SecanteMethod

    contains

    !This is the menu for all the root finding methods
    subroutine RootFinding()
        integer:: option
        print*, "Here you can choose between 4 amazing numerical methods to find the root of a non-linear function"

        do while(option /= 5) ! not equal
            print*, "Root Finding Methods"
            call funcionHumanize()
            print*, "Select one option:"
            print*, " [ 1: Bisection ] "
            print*, " [ 2: False-Position ] "
            print*, " [ 3: Newton-Raphson ] "
            print*, " [ 4: Secant ] "
            print*, " [ 5: Go back ] "
            read*, option
            call system('clear')
            select case(option)
                case (1)
                    print*, "Bisection"
                    call Bisection()
                    print*, "Remeber that the function must be in functions.f95. Check documentation if needed."
                case (2)
                    print*, "Remeber that the function must be in functions.f95. Check documentation if needed."
                    print *, "False-Position"
                    call FalsePosition()
                case (3)
                    print *, "Newton-Raphson"
                    call Newton()
                case (4)
                    print *, "Secant"
                    call Secante()
                case (5)
                    print*, "You have left the root finding module"
                    print*,""
                case default
                    print *, "The number you chose is not an option"
            end select
        end do

    end subroutine RootFinding

end module RootFindingMethods
