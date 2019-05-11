module files
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
            print*, "Up to 20 characters, please | If the file already exists there will be an error"
            read*, file_name
            ! output data into a file 
            open(1, file = 'results/'//file_name, status='new')  
                if(.not. before_max)then
                    write(1,*) "Maximum number of operations reached!!!"
                end if
                write(1,*) "Value found: ", c
                write(1,*) "Value evaluated at function: ", fc
                write(1,*) "Number of iterations: ", iter
                write(1,*) "Error: ", error
                write(1,*) "Tolerance ", tolerancia
            close(1)
        END IF
    end subroutine resultToFileROOTFIND

    subroutine resultToFileINTERPOLATION(value, sum)
        character (len = 23) :: file_name
        integer :: save_file = 0
        real:: value, sum

        print*, "Do you want to export into txt file?"
        print*, "[Type 1 to yes or 0 to continue]"
        read*, save_file
        IF(save_file == 1) THEN
            print*, "Type the file name where you want to save: "
            print*, "Up to 20 characters, please | If the file already exists there will be an error"
            read*, file_name
            ! output data into a file 
            open(1, file = 'results/'//file_name, status='new')  
                write(1,*) "The result of interpolating x=", value, "is P(", value, ")= ", sum
            close(1)
        END IF
    end subroutine resultToFileINTERPOLATION

end module files