MODULE NewtonDivededDifference
    use modulo_f

    CONTAINS

    SUBROUTINE NewtonDivided()

        real, dimension(:), allocatable::x
        real, dimension(:), allocatable::y
        real, dimension(:,:), allocatable :: aux
        integer:: n, i, j, degree
        logical:: isNotValid = .false.

        ! sum=0
        ! value=0
        ! degree=0
        ! point=0
        ! isNotValid=.true.
        
        call readPoints(x,y,n, 'inputs/Points.txt ')

        allocate(aux(n-1,n-1))

        !print *, x 
        !print *, y
        print*, "Starting"
        print*,""
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

        !print *, aux
        call askForPointsNEWTON(n, aux, x, y)


    END SUBROUTINE NewtonDivided

END MODULE NewtonDivededDifference