module outs
    contains

    subroutine resultToFileROOTFIND(c, fc, iter, error, tolerancia, before_max)
        character (len = 23) :: file_name
        integer :: save_file = 0
        real:: c, error, fc, tolerancia
        integer:: iter
        logical :: before_max

        print*, "Do you want to export into txt file?"
        print*, "[Type 1 to yes or 0 to continue]"
        read*, save_file
        IF(save_file == 1) THEN
            print*, "Type the file name where you want to save: "
            print*, "Up to 20 characters, please"
            read*, file_name
            ! output data into a file
            open(1, file = 'results/'//file_name, action='write',position='append', status='unknown')
                if(.not. before_max)then
                    write(1,*) "Maximum number of operations reached!!!"
                end if
                write(1,*) "Value found: ", c
                write(1,*) "Value evaluated at function: ", fc
                write(1,*) "Number of iterations: ", iter
                write(1,*) "Error: ", error
                write(1,*) "Tolerance ", tolerancia
            close(1, status = 'keep')
        END IF
    end subroutine resultToFileROOTFIND


    subroutine resultToFileSOLEQ(a, x, n, copyOfA, copyOfB)
        character(len = 23) :: file_name
        real, dimension(:,:), allocatable :: a, copyOfA
        real, dimension(:), allocatable :: x, copyOfB
        integer :: i, n, save_file = 0

        print*, "Type the file name where you want to save: "
        print*, "Up to 20 characters, please"
        read*, file_name
        open(1, file = 'results/'//file_name, action='write',position='append', status='unknown')
        write(1, *) "This is the system of equations you just solved:"
        do i = 1, n
            do j = 1, n-1
              write(1, "(f10.4, a2)", advance='no') copyOfA(i,j), " + "
            end do
              write(1, "(f10.4, a2)", advance='no') copyOfA(i,n), " = "
              write(1, *) copyOfB(i)
          end do
        write(1, *) "This are your results:"
        !Writes results as a list of variables
        do i = 1, n
          write(1, "(a2, i0, a3, f10.4)") "x", i, "= ", x(i)
        end do
        close (1, status = 'keep')
    end subroutine resultToFileSOLEQ


    subroutine resultToFileINTERPOLATION(value, sum)
        character (len = 23) :: file_name
        integer :: save_file = 0
        real:: value, sum

        print*, "Do you want to export into txt file?"
        print*, "[Type 1 to yes or 0 to continue]"
        read*, save_file
        IF(save_file == 1) THEN
            print*, "Type the file name where you want to save: "
            print*, "Up to 20 characters, please"
            read*, file_name
            ! output data into a file
            open(1, file = 'results/'//file_name, action='write',position='append', status='unknown')
                write(1,*) "The result of interpolating x=", value, "is P(", value, ")= ", sum
            close(1)
        END IF
    end subroutine resultToFileINTERPOLATION

    subroutine resultToFileINTERPOLNEWTON(file_name, value, sum, degree)
        character(len = 23) :: file_name
        real :: value, sum
        integer:: degree
        open(1, file = 'results/'//file_name, action='write',position='append', status='unknown')
            write(1,*) "f(", value, ")= ", sum, " | with degree = ", degree
        close(1)
    end subroutine resultToFileINTERPOLNEWTON

    subroutine resultToFileREGRESSION(file_name, value, res)
        character(len = 23) :: file_name
        real :: value, res
        open(1, file = 'results/'//file_name, action='write',position='append', status='unknown')
            write(1,*) "f(", value, ")= ", res
        close(1)
    end subroutine resultToFileREGRESSION

end module outs
