MODULE LagrangeMethod
    use modulo_f

CONTAINS

	SUBROUTINE Lagrange()

	real:: product, value, sum, limit
    real, dimension(:), allocatable::x
    real, dimension(:), allocatable::y
    integer:: n, i, j, degree, point, answer
    logical::continueL
    sum=0
    value=0
    degree=0
    point=0
    continueL=.true.
	
    print*, "Remeber that the points must be in Points.txt under the appropiate format. Check documentation if needed."
    call readPoints(x,y,n)
    
	call askForPoints(value, degree, point)
	
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
    
    call resultToFileINTERPOLATION(value, sum)

    do while(continueL)
        product=0
        sum=0
        print*, "Do you want to interpolate another point (Yes=1, No=0)"
        read*, answer
        if(answer==1) then
            print*, "Give me the new point."
            read*, value
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
              
              call resultToFileINTERPOLATION(value, sum)
        else
            continueL=.false.
        end if
    end do    


END SUBROUTINE Lagrange

END MODULE LagrangeMethod
