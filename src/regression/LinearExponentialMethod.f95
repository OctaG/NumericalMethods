MODULE LinearExponentialMethod

    use modulo_f

CONTAINS

    subroutine LinearEx()

        real:: sumX = 0, sumLnY=0, sumXLnY=0, sumXX = 0, avgX = 0, avgY = 0, a0 = 0, a1 = 0
        real:: sumSr = 0, sumSt = 0, rSquared = 0, r
        real, dimension(:), allocatable :: x
        real, dimension(:), allocatable :: y
        integer:: n, i, startPoint=0, numberPoints=0, answer
        logical:: state=.true.
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
        sumLnY=0
        sumXLnY=0
        sumXX=0
        sumSr=0
        sumSt=0

        print*, "Starting ..."
        print*, ""

         do i = startPoint, (startPoint+numberPoints-1)
            sumX = sumX + x(i)
            sumLnY = sumLnY + log(y(i))
            sumXLnY = sumXLnY + (x(i) * log(y(i)))
            sumXX = sumXX + (x(i) * x(i))
        end do

        avgX = sumX/numberPoints
        avgY = sumLnY/numberPoints

        a1 = ((numberPoints * sumXLnY) - (sumX * sumLnY))/((numberPoints * sumXX) - (sumX)**2)
        a0 = avgY - (a1 * avgX)

        do i = startPoint, (startPoint+numberPoints-1)
            sumSr = sumSr + (log(y(i)) - (a0 + a1*x(i)))**2
            sumSt = sumSt + (log(y(i))- avgY)**2
        end do

        rSquared = ABS((sumSt - sumSr) / sumSt)
        r = SQRT(rSquared)

        call outRegression(sumSr, sumSt, rSquared, r, a0, a1, state, 3)

        state=.true.
        numberPoints=0
        startPoint=0
        a0=0
        a1=0
    end subroutine LinearEx

END MODULE LinearExponentialMethod