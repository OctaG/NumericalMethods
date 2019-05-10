MODULE LinearMethod

CONTAINS

	subroutine Linear()

    real:: sumX = 0, sumY = 0, sumXY = 0, sumXX = 0, avgX = 0
    real:: avgY = 0, a0 = 0, a1 = 0, sumSr = 0, sumSt = 0
    real:: rSquared = 0, r = 0, pointToEvaluate
    real, dimension(:), allocatable :: x
    real, dimension(:), allocatable :: y
	  integer:: n, i, startPoint=0, numberPoints, cont=1, answer
      logical::isNotValid=.true., state=.true.


    print*, "Remember that the points must be in Points.txt under the appropiate format. Check documentation if needed."
	  open(7, file = 'inputs/Points2.txt')
  	read(7, *) n

	  allocate(x(n))
  	allocate(y(n))

    read(7, *) x
    read(7, *) y

	   close(7)

       print*, "Give me the number of points you want to use. They can't be less than 2 or more than ", n
    	read*, numberPoints

        if(numberPoints>n) then
			numberPoints=n
        else if(numberPoints<2) then
          numberPoints=2
        end if

        DO WHILE (startPoint < 1 .or. (n+1-startPoint)<numberPoints)
        print*, "Give me the point from which you want to start evaulating"
        read*, startPoint
        IF(startPoint < 1) THEN
            print*, "You can not give that value because the list of numbers starts at 1."
        ELSE IF((n+1-startPoint)<numberPoints) then
            print*, "You can not give that value because the remaining points are less than ", numberPoints
        END IF
   		END DO	

        print*, startPoint
        print*, numberPoints

        sumX=0
        sumY=0
        sumXX=0
        sumXY=0
        sumSr=0
        sumSt=0

		 do i = startPoint, (startPoint+numberPoints-1)
			 sumX = sumX + x(i)
			 sumY = sumY + y(i)
			 sumXY = sumXY + (x(i) * y(i))
			 sumXX = sumXX + (x(i) * x(i))
		end do

		avgX = sumX/numberPoints
		avgY = sumY/numberPoints

		a1 = ((numberPoints * sumXY) - (sumX * sumY))/((numberPoints * sumXX) - (sumX)**2)
		a0 = avgY - (a1 * avgX)

		do i = startPoint, (startPoint+numberPoints-1)
			sumSr = sumSr + (y(i) - (a0 + a1*x(i)))**2
			sumSt = sumSt + (y(i)- avgY)**2
		end do

		rSquared = ABS((sumSt - sumSr) / sumSt)
		r = SQRT(rSquared)

		print*, "The equation is: ", a0, " + ", a1, "x"
		print*, "Sr, St, r2, r: ", sumSr, sumSt, rSquared, r

        do while (state)
        print*, "Do you want to evaulate a point with the new function? (Y=1/N=0, input a number)"
        read*, answer
        if(answer==1)then
        print*, "Give me the point you want to evaluate."
        read*, pointToEvaluate
        print*, "R(x)=", (a0+(pointToEvaluate*a1))
        else
			state=.false.
        end if
		end do

		state=.true.
        isNotValid=.true.
        numberPoints=0
        startPoint=0
        pointToEvaluate=0
	end subroutine Linear

END MODULE LinearMethod