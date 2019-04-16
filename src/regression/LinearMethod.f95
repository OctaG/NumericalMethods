!MODULE LinearMethod

!CONTAINS

	program Linear

	  real:: sumX = 0, sumY = 0, sumXY = 0, sumXX = 0, avgX = 0, avgY = 0, a0 = 0, a1 = 0, sumSr = 0, sumSt = 0, rSquared = 0, r = 0
    real, dimension(:), allocatable :: x
    real, dimension(:), allocatable :: y
	  integer:: n, i


    print*, "Remeber that the points must be in Points.txt under the appropiate format. Check documentation if needed."
	  open(7, file = 'inputs/Points.txt')
  	read(7, *) n

	  allocate(x(n))
  	allocate(y(n))

    read(7, *) x
    read(7, *) y

	   close(7)

		 do i = 1, n
			 sumX = sumX + x(i)
			 sumY = sumY + y(i)
			 sumXY = sumXY + (x(i) * y(i))
			 sumXX = sumXX + (x(i) * x(i))
		end do

		avgX = sumX/n
		avgY = sumY/n

		a1 = ((n * sumXY) - (sumX * sumY))/((n * sumXX) - (sumX)**2)
		a0 = avgY - (a1 * avgX)

		do i = 1, n
			sumSr = sumSr + (y(i) - (a0 + a1*x(i)))**2
			sumSt = sumSt + (y(i)- avgY)**2
		end do

		rSquared = ABS((sumSt - sumSr) / sumSt)
		r = SQRT(rSquared)

		print*, "The equation is: ", a0, " + ", a1, "x"
		print*, "The correlation coefficient is: ", r


	end program Linear

!END MODULE LinearMethod
