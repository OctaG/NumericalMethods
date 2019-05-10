MODULE IntegrationMethod
use modulo_f

CONTAINS

	subroutine Simpson13()
    	real:: a, b, h, tol, maxIts, its, sum, oldSum, relEr
        integer:: cont
        logical:: goal

    	print*, "Give me the lower limit (a)."
    	read*, a

        print*, "Give me the upper limit (b)"
    	read*, b

        print*, "Give me the tolerance."
    	read*, tol

        print*, "Give me the maximum number of iterations."
    	read*, maxIts

        cont=2
		its=1
        goal=.false.
        oldSum=0
        relEr=0
        sum=0

        do while(.NOT.goal .and. its<maxits)
			h=(b-a)/cont
            p=a
			sum=Simpson13m(h, cont, p)
            !print *,'sum after method = ', sum
            sum = (h/3)*sum
            !print *,'sum after div = ', sum
            if(its/=1)then
              	!print *,'Before error = ', sum
                !print *,'Oldsum before error = ', oldSum
              	relEr=(sum-oldSum)/sum
                relEr=abs(relEr)
                print *,'Relative error = ', relEr
				if (relEr <= tol) then
                	goal = .true.
					print *,'Tolerance passed. Integral = ', sum
				end if
			end if
            its = its+1
			oldSum = sum
			sum = 0
            cont = cont+2
        end do

        if(.NOT.goal) then
          	print *,'Max iterations passed. Integral = ', sum
		end if


	
	end subroutine Simpson13

    subroutine Simpson38()
    	real:: a, b, h, tol, maxIts, its, sum, oldSum, relEr
        integer:: cont
        logical:: goal

    	print*, "Give me the lower limit (a)."
    	read*, a

        print*, "Give me the upper limit (b)"
    	read*, b

        print*, "Give me the tolerance."
    	read*, tol

        print*, "Give me the maximum number of iterations."
    	read*, maxIts

        cont=3
		its=1
        goal=.false.
        oldSum=0
        relEr=0
        sum=0


        do while(.NOT.goal .and. its<maxits)
			h=(b-a)/cont
            p=a
			sum=Simpson38m(h, cont, p)
            !print *,'sum after method = ', sum
            sum = (3*h/8)*sum
            !print *,'sum after div = ', sum
            if(its/=1)then
              	!print *,'Before error = ', sum
                !print *,'Oldsum before error = ', oldSum
              	relEr=(sum-oldSum)/sum
                relEr=abs(relEr)
                print *,'Relative Error = ', relEr
				if (relEr <= tol) then
                	goal = .true.
					print *,'Tolerance passed. Integral = ', sum
				end if
			end if
            its = its+1
			oldSum = sum
			sum = 0
            cont = cont+3
        end do

        if(.NOT.goal) then
          	print *,'Max iterations passed. Integral = ', sum
		end if

	
	end subroutine Simpson38


    subroutine Trapezoidal()
    	real:: a, b, h, tol, maxIts, its, sum, oldSum, relEr
        integer:: cont
        logical:: goal

    	print*, "Give me the lower limit (a)."
    	read*, a

        print*, "Give me the upper limit (b)"
    	read*, b

        print*, "Give me the tolerance."
    	read*, tol

        print*, "Give me the maximum number of iterations."
    	read*, maxIts

        cont=1
		its=1
        goal=.false.
        oldSum=0
        relEr=0
        sum=0

        print *, .NOT.goal

        do while(.NOT.goal .and. its<maxits)
			h=(b-a)/cont
            p=a
			sum=Trapm(h, cont, p)
            !print *,'sum after method = ', sum
            sum = (h/2)*sum
            !print *,'sum after div = ', sum
            if(its/=1)then
              	!print *,'Before error = ', sum
                !print *,'Oldsum before error = ', oldSum
              	relEr=(sum-oldSum)/sum
                relEr=abs(relEr)
                print *,'Relative Error = ', relEr
				if (relEr <= tol) then
                	goal = .true.
					print *,'Tolerance passed. Integral = ', sum
				end if
			end if
            its = its+1
			oldSum = sum
			sum = 0
            cont = cont+1
        end do

        if(.NOT.goal) then
          	print *,'Max iterations passed. Integral = ', sum
		end if

	
	end subroutine Trapezoidal

    function Simpson13m(h, cont, p)
    	real:: sum, h, p
        integer::i, cont
		i=1
        sum=0
        !print *,'h = ', h
		do i=1, cont/2
			sum = sum + funcionIntegral(p)+4*funcionIntegral(p+h) + funcionIntegral(p+2*h)
            p=p+2*h
        end do
        !print *,'sum in method = ', sum
        Simpson13m=sum

    end function Simpson13m

    function Simpson38m(h, cont, p)
    	real:: sum, h, p
        integer::i, cont
		i=1
        sum=0
        !print *,'h = ', h
		do i=1, cont/3
			sum = sum + funcionIntegral(p)+3*funcionIntegral(p+h) + 3*funcionIntegral(p+2*h) + funcionIntegral(p+3*h)
            p=p+3*h
        end do
        !print *,'sum in method = ', sum
        Simpson38m=sum

    end function Simpson38m

    function Trapm(h, cont, p)
    	real:: sum, h, p
        integer::i, cont
		i=1
        sum=0
        !print *,'h = ', h
		do i=1, cont
			sum = sum + funcionIntegral(p)+ funcionIntegral(p+h)
            p=p+h
        end do
        !print *,'sum in method = ', sum
        Trapm=sum

    end function Trapm

    
END MODULE IntegrationMethod