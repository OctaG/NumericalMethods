MODULE PowerSeriesMethod

CONTAINS

	SUBROUTINE PowerSeries()

	real:: product, value, sum, limit
    real, dimension(:), allocatable::x
    real, dimension(:), allocatable::y
    real, dimension(:,:), allocatable::matrix!10
	integer:: n, i, j, degree, point, size, pointT !11
    logical:: isNotValid
    sum=0
    value=0
    degree=0
    point=1
    isNotValid=.true.
	
	open(7, file = 'Points.txt')
  	read(7, *) n!20
	
	allocate(x(n))
  	allocate(y(n))

    read(7, *) x
    read(7, *) y

	close(7)
    
	print*, "Give me the point you want evaluated."!30
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
        print*, matrix(i,1)!60
    end do
	
print*, size
pointT=point

    do i=1, size
      
      print*, "I ",i
		do j=2, size
        	print*, "Point T",pointT
			matrix(i,j)=x(pointT)**(j-1)
            print*, "J ",j
    	end do

        pointT=pointT+1
    end do

    print*, matrix

    open(8, file="PowerSeriesMatrix.txt")!70
    write(8, *) size
    do i=1, size
		write(8, *) (matrix(i, j), j=1, size)
    end do
    write(8, *) (y(i), i=point, point+degree)
    close(8)

END SUBROUTINE PowerSeries

END MODULE PowerSeriesMethod
