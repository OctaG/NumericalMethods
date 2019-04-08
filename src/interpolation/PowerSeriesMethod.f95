MODULE PowerSeriesMethod

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
    point=1
    isNotValid=.true.
	
	open(7, file = 'Points.txt')
  	read(7, *) n
	
	allocate(x(n))
  	allocate(y(n))

    read(7, *) x
    read(7, *) y

	close(7)
    
	print*, "Give me the point you want evaluated."
    read*, value

	print*, "Give me the degree of the polynomial."
    read*, degree

    print*, "Give me the point from which you want to start evaulating."
    read*, point
	

	DO WHILE (isNotValid)!40
    limit=point+degree
    IF((limit)>n) THEN
      	print*, "The values for degree and initial point are not valid."
		print*, "Give me the degree of the polynomial."
    	read*, degree

    	print*, "Give me the point from which you want to start evaulating"
    	read*, point
    ELSE
      	isNotValid=.false.!50
    END IF
    END DO
	
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

    print*, matrix
    
	allocate(aResults(degree+1))
    allocate(y2(degree+1))

	do i=point, point+degree
		y2(cont)=y(i)
        cont=cont+1
    end do
    print*, y2
    n2=degree+1
	call gaussP(matrix, y2, aResults, n2)
	
	
    resultP=0
    
		do i=1, degree+1
			resultP=resultP+aResults(i)*value**(i-1)	
        end do
        

    print*, resultP
    print*, "Result written in PowerSeriesFinalOutcome.txt"

    open(11, file="PowerSeriesFinalOutcome.txt")
    write(11, *) "The result of interpolating x=", value, "is P(", value, ")= ", resultP
    close(11)

END SUBROUTINE PowerSeries

SUBROUTINE gaussP(matrix, y2, aResults, n)
  integer:: n
  real, dimension(:,:):: matrix
  real, dimension(:) :: y2
  real, dimension(:) :: aResults
  real:: sum
  	
	print*, matrix
    !matrix = transpose(matrix)
    print*, matrix
    print*, y2

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
  
	print*, aResults
end subroutine gaussP

END MODULE PowerSeriesMethod
