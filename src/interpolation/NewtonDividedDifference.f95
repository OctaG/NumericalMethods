MODULE NewtonDivededDifference

    CONTAINS

    SUBROUTINE NewtonDivided()

        real:: value, sum, mtemp
        real, dimension(:), allocatable::x
        real, dimension(:), allocatable::y
        real, dimension(:,:), allocatable :: aux
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
        read(7, *) x
        read(7, *) y
        close(7)

        allocate(aux(n-1,n-1))

        !print *, x 
        !print *, y

        do i = 1, n-1, 1
            !run n-1 times, becase it is inclusive
            !this for if the colums
            do j = 1, n - i, 1
               !this is in the rows
               !print *, i, ",",  j
               !print *, y(i+1), "-", y(i), "/", x(i+1), "-", x(i)
               if ( i .eq. 1 ) then
                    aux(i,j) = (y(j+1) - y(j)) / (x(j+i) - x(j))
               else
                    !print *, aux(i-1,j+1), "-", aux(i-1,j), "/", x(j+i), "-", x(j)
                    aux(i,j) = (aux(i-1,j+1) - aux(i-1,j)) / (x(j+i) - x(j))
               end if
               !aux(i,j) = 10*i
            end do
        end do

        print *, aux

        print *, "Give me the value of x in f(x) you want to evaluate"
        read*, value

        sum = y(1)
        do i = 1, n-1, 1
            mtemp = aux(i,1)
            do j = 1, i, 1
                !print *, "x", j
                mtemp = mtemp * (value - x(j))
            end do
            sum = sum + mtemp
        end do
        print *, sum

        print*, "Result is written in newtondivided.txt"

        open(8, file="newtondivided.txt")
        write(8, *) "The result of interpolating x=", value, "is P(", value, ")= ", sum
        close(8)
    END SUBROUTINE NewtonDivided

END MODULE NewtonDivededDifference