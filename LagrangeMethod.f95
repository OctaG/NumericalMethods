MODULE LagrangeMethod

CONTAINS

	SUBROUTINE Lagrange()

	real:: product, value, sum, limit
    real, dimension(:), allocatable::x
    real, dimension(:), allocatable::y
	integer:: n, i, j, degree, point
    logical:: isNotValid
    sum=0
    value=0
    degree=0
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

	print*, "Give me the degree of the polynomial"
    read*, degree

    print*, "Give me the point from which you want to start evaulating"
    read*, point
	

	DO WHILE (isNotValid)
    limit=point+degree
    IF((limit)>n) THEN
      	print*, "The values for degree and initial point are not valid"
		print*, "Give me the degree of the polynomial"
    	read*, degree

    	print*, "Give me the point from which you want to start evaulating"
    	read*, point
    ELSE
      	isNotValid=.false.
    END IF
    END DO
	
    do i=point, degree+point
      product = y(i)

      do j=point, degree+point
		IF(i /= j) THEN
			product = product * (value-x(j))/(x(i)-x(j))
        END IF
      end do
		sum=sum+product
    end do
    
	print*, sum

    open(8, file="LagrangeOutcome.txt")
    write(8, *) "The result of interpolating x=", value, "is P(", value, ")= ", sum
    close(8)

END SUBROUTINE Lagrange

END MODULE LagrangeMethod