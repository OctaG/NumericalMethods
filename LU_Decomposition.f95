program LU_Decomposition

  integer:: n
  real, dimension(:,:), allocatable :: a
  real, dimension(:), allocatable :: b
  real, dimension(:), allocatable :: x
  real:: sum

  open(1, file = 'myData.txt')
  read(1, *) n

  open(2, file = 'results.txt')

  allocate(a(n,n))
  allocate(b(n))
  allocate(x(n))

  read(1, *) a
  a = transpose(a)

  read(1, *) b

  !Decomposition phase using crout
  do j = 2, n
    a(1,j) = a(1,j) / a(1,1)
  end do

  do j = 2, n - 1
    do i = j, n
       sum = 0
       do k = 1, j- 1
         sum = sum + a(i,k) * a(k,j)
       end do
       a(i,j) = a(i,j) - sum
     end do
       do k = j + 1, n
         sum = 0
         do i = 1, j - 1
           sum = sum + a(j,i) * a(i,k)
         end do
         a(j,k) = (a(j,k) - sum)/a(j,j)
       end do
  end do
  sum = 0

  do k = 1, n - 1
    sum = sum + a(n,k) * a(k,n)
  end do
  a(n,n) = a(n,n) - sum


  !Forwards substitution
  b(1) = b(1)/a(1,1)
  do i = 2, n
    sum = b(i)
    do j = 1, i - 1
      sum = sum - (a(i,j) * b(j))
    end do
    b(i) = sum / a(i,i)
  end do

  !Backwards substitution
  x(n) = b(n)

  do i = n - 1, 1, -1
    sum = 0
    do j = i + 1, n
      sum = sum + (a(i,j) * x(j))
    end do
    x(i) = (b(i) - sum)
  end do

  a = transpose(a)
  write(2, *) a
  write(2, *) "This are your results:"
  write(2, *) nint(x)

end program LU_Decomposition
