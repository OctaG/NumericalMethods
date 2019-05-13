module LU_DecompositionMethod

  !Modules used
  use modulo_f

  contains

    subroutine LU_Decomposition()

      integer:: n, continue
      real, dimension(:,:), allocatable :: a, copyOfA
      real, dimension(:), allocatable :: b, copyOfB
      real, dimension(:), allocatable :: x
      real:: sum
      character(len = 23) :: file_name   
      file_name = "myData.txt"
      call askInpuFile(file_name)
      continue = 1
      call writeFileToMatrix(n, a, b, x, copyOfA, copyOfB,file_name)
      print*, "Starting ..."
      print*, ""

      !Decomposition phase using crout
      do j = 2, n
        a(1,j) = a(1,j) / a(1,1)
      end do

      do j = 2, n - 1
        do i = j, n
           sum = 0
           do k = 1, j- 1
             sum = sum + a(i,k) * a(k,j)
           end do
           a(i,j) = a(i,j) - sum
         end do
           do k = j + 1, n
             sum = 0
             do i = 1, j - 1
               sum = sum + a(j,i) * a(i,k)
             end do
             a(j,k) = (a(j,k) - sum)/a(j,j)
           end do
      end do
      sum = 0

      do k = 1, n - 1
        sum = sum + a(n,k) * a(k,n)
      end do
      a(n,n) = a(n,n) - sum

     do while(continue /= 0)
        !Forwards substitution
        b(1) = b(1)/a(1,1)
        do i = 2, n
          sum = b(i)
          do j = 1, i - 1
            sum = sum - (a(i,j) * b(j))
          end do
          b(i) = sum / a(i,i)
        end do

        !Backwards substitution
        x(n) = b(n)

        do i = n - 1, 1, -1
          sum = 0
          do j = i + 1, n
            sum = sum + (a(i,j) * x(j))
          end do
          x(i) = (b(i) - sum)
        end do

        call writeResultsToFile(a, x, n, copyOfA, copyOfB)

        print*, "Would you like to solve for a different RHS [yes = 1/ no = 0]"
        print*, "Remember RHS should be in RHS.txt"
        read*, continue
        call writeRHSToMatrix(b)
        copyOfB = b
      end do
      call system('clear')
      print*, "Complete..."
      print*, ""
    end subroutine LU_Decomposition

end module LU_DecompositionMethod
