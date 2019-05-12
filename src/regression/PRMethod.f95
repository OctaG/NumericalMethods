MODULE PRMethod

use modulo_f

CONTAINS

    subroutine PRegression()

        real:: sumX = 0, sumY = 0, sumXY=0, avgY = 0, sumSr = 0, sumSt = 0, rSquared = 0, r =0, newPoint=0, pointToEvaluate
        real, dimension(:), allocatable :: x
        real, dimension(:), allocatable :: xSum
        real, dimension(:), allocatable :: y
        real, dimension(:), allocatable :: b
        real, dimension(:), allocatable :: newPoints
        real, dimension(:), allocatable:: aResults
        real, dimension(:,:), allocatable::matrix
        character (len = 23) :: file_name
        integer:: n, i, degree, size, j, k, k2, startPoint=0, numberPoints, cont=1, answer
        logical::isNotValid=.true., state=.true.

        call readPoints(x,y,n, 'inputs/Points2.txt')

        degree = -1

        do while(degree <= 1)
            print*, "Give me the degree of the polynomial."
            read*, degree
            if(degree == 1) then
                print*, "Use linear"
            else
                print*, "Must be natural"
            end if
        end do

        print*, "Give me the number of points you want to use. They can't be more than ", n
        read*, numberPoints

        if(numberPoints>n) then
            numberPoints=n
        end if

        do while(isNotValid)
            if(numberPoints<(degree+1)) then
                print*, "The number of points can't be smaller than the degree plus one. Try again."
                print*, "Give me the degree of the polynomial."
                read*, degree
                print*, "Give me the number of points you want to use."
                read*, numberPoints
                if(numberPoints>n) then
                    numberPoints=n
                end if
            else
              isNotValid=.false.
            end if
            
        end do
        
        DO WHILE (startPoint < 1 .or. (n+1-startPoint)<numberPoints)
        print*, "Give me the point from which you want to start evaulating"
        read*, startPoint
        IF(startPoint < 1) THEN
            print*, "You can not give that value because the list of numbers starts at 1"
        ELSE IF((n+1-startPoint)<numberPoints) then
            print*, "You can not give that value because the remaining points are less than ", numberPoints
        END IF
           END DO	

        size=degree+1
    
        allocate(matrix(size,size))
        allocate(aResults(size))
        allocate(b(size))
        allocate(xSum(degree*2))
        allocate(newPoints(numberPoints))

        sumY=0
        newPoint=0

        print*, "Starting ..."
        print*, ""

        do i = startPoint, (startPoint+numberPoints-1)
             sumY = sumY + y(i)
        end do

        avgY = sumY/numberPoints
        
        do i = 1, degree*2
          do j = startPoint, (startPoint+numberPoints-1)
             sumX = sumX + x(j)**i
          end do
          xSum(i)=sumX
          sumX=0
        end do

        !print*, xSum

        matrix(1,1)=numberPoints

        do i = 1, size
          do j = startPoint, (startPoint+numberPoints-1)
             sumXY = sumXY + (y(j)*x(j)**(i-1))
          end do
          b(i)=sumXY
          sumXY=0
        end do
        
        !print*, xSum

        k=0
        do i=1, size
              k2=k
            do j=1, size
                IF(i==1 .and. j==1) then
                else
                    matrix(i,j)=xSum(k2)
                end if
                k2=k2+1
            end do
            k=k+1
        end do

        call gaussP(matrix, b, aResults, size)
        

        do i = startPoint, (startPoint+numberPoints-1)
              do j=1, size
                newPoint=newPoint+(aResults(j)*x(i)**(j-1))
            end do
            !print*, i
            newPoints(cont)=newPoint
            newPoint=0
            cont=cont+1
            !print*, cont
        end do
        cont=1
        
        sumSr=0
        sumSt=0
        rSquared=0
        r=0
        
        !print*, sumSr
        !print*, sumSt
        !print*, newPoints
        do i = startPoint, (startPoint+numberPoints-1)
            sumSr = sumSr + (y(i) - (newPoints(cont)))**2
            sumSt = sumSt + (y(i)- avgY)**2
            cont=cont+1
        end do
    
        !print*, sumSr
        !print*, sumSt
        !print*, avgY
        
        rSquared = ABS((sumSt - sumSr) / sumSt)
        r = SQRT(rSquared)

        print*, "Type the file name where you want to save: "
        print*, "Up to 20 characters, please"
        read*, file_name
        open(1, file = 'results/'//file_name, action='write',position='append', status='unknown')
            write(1, *) "Remember that equations have the form: A0 + A1x^1 + ... + Anx^n"
            write(1, *) "The A's from A0 to An of the polynomial regression are "
            write(1, *) aResults
            write(1, *) "Sr = ", sumSr
            write(1, *) "St = ", sumSt
            write(1, *) "r^2 = ", r2
            write(1, *) "r = ", r
            write(1, *) ""
        close(1)

        print*, "Sr = ", sumSr
        print*, "St = ", sumSt
        print*, "r^2 = ", rSquared
        print*, "r = ", r
        
        print*, "Remember that equations have the form: A0 + A1x^1 + ... + Anx^n"
        print*, "The A's from A0 to An of the polynomial regression are "
        print*, aResults
        
        do while (state)
            print*, "Do you want to evaulate a point with the new equation? (Y=1/N=0, input a number)"
            read*, answer
            if(answer==1) then
                newPoint = 0
                print*, "Give me the point you want to evaluate."
                read*, pointToEvaluate
                do j=1, size
                    newPoint=newPoint+(aResults(j)*pointToEvaluate**(j-1))
                end do
                print*, "R(x)=", newPoint
                call resultToFileREGRESSION(file_name, pointToEvaluate, newPoint)
             else
                state=.false.
             end if
        end do

        startPoint=0
        cont=1
        state=.true.
        isNotValid=.true.
        file_name = ''

        call system('clear')
        print*, "Complete..."
        print*, ""
    end subroutine PRegression


    SUBROUTINE gaussP(matrix, y2, aResults, n)
  integer:: n
  real, dimension(:,:):: matrix
  real, dimension(:) :: y2
  real, dimension(:) :: aResults
  real:: sum
      

  !Forward elimination
  do k = 1, n-1
    do i = k + 1, n
      matrix(i,k) = matrix(i,k)/matrix(k,k)
      do j = k + 1, n
        matrix(i,j) = matrix(i,j) - matrix(i,k) * matrix(k,j)
      end do
      y2(i) = y2(i) - matrix(i,k) * y2(k);
    end do
  end do

  aResults(n) = y2(n)/matrix(n,n)

  !Backwards substitution
  do i = n-1, 1, -1
    sum = y2(i)
    do j = i + 1, n
      sum = sum - matrix(i,j)* aResults(j)
    end do
    aResults(i) = sum/matrix(i,i)
  end do
  

end subroutine gaussP

END MODULE PRMethod
