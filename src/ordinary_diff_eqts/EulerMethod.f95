module EulerMethod
use modulo_f
contains
  subroutine Euler()

    integer:: numinterval
    real:: tol
    real:: a
    real:: b
    real:: fa
    integer:: fixed
    integer:: maxits
    real::k1
    integer:: converged = 0

  	Call in_data (numinterval,tol,a,b,fa,fixed,maxits)

    print*, "Euler"
  	if (fixed == 1) then
  	   h=(b-a)/numinterval
  		 xold=a
  		 yold=fa

  		 print *,'          x            y'
  		 do i = 1, numinterval
  		     xnew = xold + h
           k1 = fdexy(xold,yold)
           ynew = yold + h * k1
           print*, ynew
           print *, xnew, ynew
  				 xold = xnew
  				 yold = ynew
  			end do
  			write (*,*) 'press 1 to go back or 0 to finish'
  			read (*,*) cont
  		else
  			h=b-a
  			its=1
  			intercount=1
  			xold=a
  			yold=fa
  			yorig = fa + h*fdexy(a,fa)
  			print *,'value at x = ',b,' using biggest interval = ',yorig
  			do while (converged == 0 .and. its < maxits)
  			     intercount=intercount*2
  					hnew=h/intercount
  					do while (xold < b)
  						xnew = xold + hnew
              k1 = fdexy(xold,yold)
              ynew = yold + hnew * k1
  						xold = xnew
  						yold = ynew
  					end do
  					errorrel = abs((yold-yorig)/yold)
  					if (errorrel <= tol) converged = 1
  					its = its + 1
  					yorig = yold
  					xold = a
  					yold = fa
  				end do
  				its = its -1
  				print *, 'y at x = ',b,' = ',yorig, ' using an h = ',hnew
  				print *, 'with a relative error of ',errorrel, 'after ',its,' iterations'
  				write (*,*) 'press 1 to go back or 0 to finish'
  				read (*,*) cont
  			endif
    end subroutine Euler
end module EulerMethod
