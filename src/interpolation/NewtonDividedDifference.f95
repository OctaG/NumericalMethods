MODULE NewtonDivededDifference

    CONTAINS

    SUBROUTINE NewtonDivided()

        real:: value
        real, dimension(:), allocatable::x
        real, dimension(:), allocatable::y
        real, dimension(:,:), allocatable :: a
        integer:: n, i, j
        ! logical:: isNotValid
        ! sum=0
        ! value=0
        ! degree=0
        ! point=0
        ! isNotValid=.true.
        
        print*, "Remeber that the points must be in Points.txt under the appropiate format. Check documentation if needed."
        open(7, file = 'inputs/Points.txt')
        read(7, *) n
        
        allocate(x(n))
        allocate(y(n))
        allocate(a(n-1,n-1))

        read(7, *) x
        read(7, *) y

        close(7)

        print *, x
        print *, y
        print *, n
        print *, a
        
        print *, "Give me the value of x in f(x) you want to evaluate"
        read*, value

    END SUBROUTINE NewtonDivided

END MODULE NewtonDivededDifference