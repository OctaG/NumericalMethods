module GaussianEliminationMethod

  !Modules used
  use modulo_f

    contains

    subroutine GaussianElimination()
        integer:: n
        real, dimension(:,:), allocatable :: a, copyOfA
        real, dimension(:), allocatable :: b, copyOfB
        real, dimension(:), allocatable :: x
        real:: sum
        character(len = 23) :: file_name   
        file_name = "myData.txt"
        call askInpuFile(file_name)
    
        call writeFileToMatrix(n, a, b, x, copyOfA, copyOfB,file_name)
        print*, "Starting ..."
        print*, ""
        !Forward elimination
        do k = 1, n-1
          do i = k + 1, n
            a(i,k) = a(i,k)/a(k,k)
            do j = k + 1, n
              a(i,j) = a(i,j) - a(i,k) * a(k,j)
            end do
            b(i) = b(i) - a(i,k) * b(k);
          end do
        end do

        x(n) = b(n)/a(n,n)

        !Backwards substitution
        do i = n-1, 1, -1
          sum = b(i)
          do j = i + 1, n
            sum = sum - a(i,j)* x(j)
          end do
          x(i) = sum/a(i,i)
        end do

        call writeResultsToFile(a, x, n, copyOfA, copyOfB)
        call system('clear')
        print*, "Complete..."
        print*, ""

    end subroutine GaussianElimination

end module GaussianEliminationMethod
