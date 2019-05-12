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
        option = -1
        do while(option /= 5) ! not equal
            print*, "=== Root Finding Methods ==="
            call funcionHumanize()
            print*,""
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
                    print*, "-- Remember that the function must be in functions.f95."
                    print*, "-- Check documentation if needed."
                    print*,""
                    call Bisection()
                case (2)
                    print *, "False-Position"
                    print*, "-- Remember that the function must be in functions.f95."
                    print*, "-- Check documentation if needed."
                    print*,""
                    call FalsePosition()
                case (3)
                    print *, "Newton-Raphson"
                    print*, "-- Remember that the function must be in functions.f95 and the repective derivate."
                    print*, "-- Check documentation if needed."
                    print*,""
                    call Newton()
                case (4)
                    print *, "Secant"
                    print*, "-- Remember that the function must be in functions.f95 and the repective derivate."
                    print*, "-- Check documentation if needed."
                    print*,""
                    call Secante()
                case (5)
                    print*, "You have left the root finding module"
                    print*,""
                    print*,""
                case default
                    print *, "The number you chose is not an option"
                    print*,""
            end select
        end do

    end subroutine RootFinding

end module RootFindingMethods
