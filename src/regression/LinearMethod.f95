MODULE LinearMethod

use modulo_f

CONTAINS

    subroutine Linear()

        real:: sumX = 0, sumY = 0, sumXY = 0, sumXX = 0, avgX = 0
        real:: avgY = 0, a0 = 0, a1 = 0, sumSr = 0, sumSt = 0
        real:: rSquared = 0, r = 0, pointToEvaluate
        real, dimension(:), allocatable :: x
        real, dimension(:), allocatable :: y
        integer:: n, i, startPoint=0, numberPoints, cont=1, answer
        logical::isNotValid=.true., state=.true.
        character(len = 23) :: file_name   
        file_name = "Points.txt"
        
        call readPoints(x,y,n, file_name)
        numberPoints=0
        call askForNumberOfPoints(numberPoints, n)
        startPoint=0
        call askForStartingPoint(startPoint, numberPoints, n)	

        !print*, startPoint
        !print*, numberPoints

        sumX=0
        sumY=0
        sumXX=0
        sumXY=0
        sumSr=0
        sumSt=0

        print*, "Starting ..."
        print*, ""

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

        call outRegression(sumSr, sumSt, rSquared, r, a0, a1, state, 1)

        state=.true.
        isNotValid=.true.
        numberPoints=0
        startPoint=0
        pointToEvaluate=0
    end subroutine Linear

END MODULE LinearMethod