module RungeKutta3Method
use modulo_f
contains
  subroutine RungeKutta3()
      integer:: numinterval
      real:: tol
      real:: a
      real:: b
      real:: fa
      integer:: fixed
      integer:: maxits
      real::k1, k2, k3
      integer:: converged = 0
      character (len = 23) :: file_name

    Call in_data (numinterval,tol,a,b,fa,fixed,maxits)
    print*, "Finally type the file name where you want to save: "
    print*, "Up to 20 characters, please"
    read*, file_name
    open(1, file = 'results/'//file_name, action='write',position='append', status='unknown')

    if (fixed == 1) then
      h=(b-a)/numinterval
      xold=a
      yold=fa

      print *,'          x            y'
      write(1,*) '          x            y'
      do i = 1,numinterval
        xnew = xold + h

                  k1 = fdexy(xold,yold)
        k2 = fdexy(xold+(h/2),yold+(h/2)*k1)
        k3 = fdexy(xnew,yold-h*k1+2*h*k2)

        ynew = yold + (h/6)*(k1+4*k2+k3)
                  print*, k1
                  write(1,*) k1
                  print*, k2
                  write(1,*) k2
                  print*, k3
                  write(1,*) k3
                  print*, ynew
                  write(1,*) ynew
        print *, xnew, ynew
        write(1,*) xnew, ynew
        xold = xnew
        yold = ynew
      end do
      !write (*,*) 'press 1 to go back or 0 to finish'
      !read (*,*) cont
    else
      h=b-a
      its=1
      intercount=1
      xold=a
      yold=fa
      yorig = fa + h*fdexy(a,fa)
      print *,'value at x = ',b,' using biggest interval = ',yorig
      write(1,*) 'value at x = ',b,' using biggest interval = ',yorig
      do while (converged == 0 .and. its < maxits)
        intercount=intercount*2
        hnew=h/intercount
        do while (xold < b)
          xnew = xold + hnew
          k1 = fdexy(xold,yold)
          k2 = fdexy(xold+(hnew/2),yold+(hnew/2)*k1)
          k3 = fdexy(xnew,yold-hnew*k1+2*hnew*k2)
          ynew = yold + (hnew/6)*(k1+4*k2+k3)
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
      write(1,*) 'y at x = ',b,' = ',yorig, ' using an h = ',hnew
      print *, 'with a relative error of ',errorrel, 'after ',its,' iterations'
      write(1,*) 'with a relative error of ',errorrel, 'after ',its,' iterations'
      !write (*,*) 'press 1 to go back or 0 to finish'
      !read (*,*) cont
    end if
    write(1,*) ""
    close(1)
    print*, ""
    !call system('clear')
    print*, "Complete..."
    print*, ""
  end subroutine RungeKutta3
end module RungeKutta3Method
