MODULE LagrangeMethod
    use modulo_f
    use outs

CONTAINS

	SUBROUTINE Lagrange()

	real:: product, value, sum
    real, dimension(:), allocatable::x
    real, dimension(:), allocatable::y
    real, dimension(:), allocatable::coef
    integer:: n, i, j, degree, point, answer, coefCont
    logical::continueL
    sum=0
    value=0
    degree=0
    point=0
    continueL=.true.
    coefCont=1
	
    call readPoints(x,y,n, 'inputs/Points.txt ')
    
	call askForPoints(value, degree, point, n)
	
    allocate(coef(degree+1))
	
    do i=point, degree+point
      product = 1

      do j=point, degree+point
		IF(i /= j) THEN
			product = product * (value-x(j))/(x(i)-x(j))
        END IF
      end do
      	coef(coefCont)=product
      	product = y(i)*product
		sum=sum+product
        coefCont=coefCont+1
    end do
    
    call resultToFileINTERPOLATIONLagrange(value, sum, coef, degree, point, x, y)
    call system('clear')
    print*, "Complete..."
    print*, ""


END SUBROUTINE Lagrange

END MODULE LagrangeMethod
