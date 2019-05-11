MODULE PowerSeriesMethod
    use files
    use modulo_f

CONTAINS

	SUBROUTINE PowerSeries()

	real:: product, value, sum, limit, resultP
    real, dimension(:), allocatable::x
    real, dimension(:), allocatable::y
    real, dimension(:), allocatable::y2
    real, dimension(:), allocatable::aResults
    real, dimension(:,:), allocatable::matrix
	integer:: n, i, j, degree, point, size, pointT, cont, n2
    logical:: isNotValid
    sum=0
    value=0
    degree=0
    cont=1
    point=0
    isNotValid=.true.
    
    print*, "Remeber that the points must be in Points.txt under the appropiate format. Check documentation if needed."
    call readPoints(x,y,n)
    
	call askForPoints(value, degree, point)
	
	size=degree+1

    allocate(matrix(size,size))

    do i=1, size
		matrix(i,1)=1
        !print*, matrix(i,1)!60
    end do
	
	pointT=point

    do i=1, size
		do j=2, size
			matrix(i,j)=x(pointT)**(j-1)
    	end do

        pointT=pointT+1
    end do
    
	allocate(aResults(degree+1))
    allocate(y2(degree+1))

	do i=point, point+degree
		y2(cont)=y(i)
        cont=cont+1
    end do
    


    n2=degree+1
	call gaussP(matrix, y2, aResults, n2)
	
	
    resultP=0
    
		do i=1, degree+1
			resultP=resultP+aResults(i)*value**(i-1)	
        end do
        

    print*, resultP
    print*, "Result written in PowerSeriesFinalOutcome.txt"

    call resultToFileINTERPOLATION(value, sum)

END SUBROUTINE PowerSeries

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

END MODULE PowerSeriesMethod
