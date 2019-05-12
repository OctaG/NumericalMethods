module GaussSeidelMethod

!Needs to tell the user equations must be dominant diagonal
  !Modules used
  use modulo_f
  contains

      subroutine GaussSeidel()
        integer:: n, max
        real, dimension(:,:), allocatable :: a, copyOfA
        real, dimension(:), allocatable :: b, copyOfB
        real, dimension(:), allocatable :: x
        real:: sum, lambda = 1.0, tolerance, isolated

        !Lambda can be stablished different if relaxation is needed
        call writeFileToMatrix(n, a, b, x, copyOfA, copyOfB)
        call askForStopValues(tolerance, max)
        print*, "Starting ..."
        print*, ""

        !First iteration x's = 0
       		do i = 1, n
            x(i) = 0
          end do


        !Isolation
        do i = 1, n
          isolated = a(i,i)
          do j = 1, n
            a(i,j) = a(i,j)/isolated
          end do
          b(i) = b(i)/isolated
        end do

        !New x's
        do i = 1, n
          sum = b(i)
          do j = 1 , n
            if(i/=j)then
              sum = sum - a(i,j)* x(j)
            end if
          end do
          x(i) = sum
        end do

		    !Error calculation
        iter = 1
        do
          sentinel = 1
          do i = 1, n
            old = x(i)
            sum = b(i)
            do j = 1, n
              if (i/=j)then
                sum = sum - a(i,j) * x(j)
              end if
            end do
            x(i) = lambda*sum + (1-lambda)*old

			      if((sentinel == 1) .AND. (x(i)/=0)) then
              ea = ABS((x(i) - old) / (x(i)))
              if(ea > tolerance) then
                sentinel = 0
              end if
            end if
          end do
       		iter = iter + 1
          if(sentinel == 1 .OR. iter>=max) then
            exit
          end if
        end do

        call writeResultsToFile(a, x, n, copyOfA, copyOfB)
        call system('clear')
        print*, "Complete..."
        print*, 

      end subroutine GaussSeidel

end module GaussSeidelMethod
