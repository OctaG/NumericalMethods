module GaussforPower

CONTAINS

SUBROUTINE gaussP()
  integer:: n
  real, dimension(:,:), allocatable :: a
  real, dimension(:), allocatable :: b
  real, dimension(:), allocatable :: x
  real:: sum
	
    open(5, file = 'PowerSeriesMatrix.txt')
    read(5, *) n
   
	allocate(a(n,n))
    allocate(b(n))
    allocate(x(n))
    read(5, *) a
    a = transpose(a)

    read(5, *) b
	
    
    close(5)
  !Forward elimination
  do k = 1, n-1
    do i = k + 1, n
      a(i,k) = a(i,k)/a(k,k)
      do j = k + 1, n
        a(i,j) = a(i,j) - a(i,k) * a(k,j)
      end do
      b(i) = b(i) - a(i,k) * b(k);
    end do
  end do

  x(n) = b(n)/a(n,n)

  !Backwards substitution
  do i = n-1, 1, -1
    sum = b(i)
    do j = i + 1, n
      sum = sum - a(i,j)* x(j)
    end do
    x(i) = sum/a(i,i)
  end do

  open(6, file = 'ResultsPowerSeries.txt')

    write(6, *) (x(i), i=1, n)

  close(6)
end subroutine gaussP

end module GaussforPower