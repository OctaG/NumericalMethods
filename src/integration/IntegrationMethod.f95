MODULE IntegrationMethod
    use modulo_f

CONTAINS

    subroutine Simpson13()
        real:: a, b, h, tol, maxIts, its, sum, oldSum, relEr
        integer:: cont
        logical:: goal

        call askForLimits(a, b, tol, maxIts)

        cont=2
        its=0
        goal=.false.
        oldSum=0
        relEr=0
        sum=0



        print*, "Starting ..."
        print*, ""
        call funcionIntegralHumanize()

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
                    print *,'Tolerance passed'
                    print*, 'Integral = ', sum
                    call outIntegral(sum, relEr, tol, .false.)
                end if
            end if
            its = its+1
            oldSum = sum
            sum = 0
            cont = cont+2
        end do

        if(.NOT.goal) then
            print *,'Max iterations passed.'
            print*, 'Integral = ', oldSum
            call outIntegral(oldSum, relEr, tol, .false.)
        end if

        call system('clear')
        print*, "Complete..."
        print*, ""

    end subroutine Simpson13

    subroutine Simpson38()
        real:: a, b, h, tol, maxIts, its, sum, oldSum, relEr
        integer:: cont
        logical:: goal

        call askForLimits(a, b, tol, maxIts)

        cont=3
        its=0
        goal=.false.
        oldSum=0
        relEr=0
        sum=0

        print*, "Starting ..."
        print*, ""
        call funcionIntegralHumanize()


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
                    print *,'Tolerance passed'
                    print*, 'Integral = ', sum
                    call outIntegral(sum, relEr, tol, .false.)
                end if
            end if
            its = its+1
            oldSum = sum
            sum = 0
            cont = cont+3
        end do

        if(.NOT.goal) then
            print *,'Max iterations passed.'
            print*, 'Integral = ', oldSum
            call outIntegral(oldSum, relEr, tol, .false.)
        end if

        call system('clear')
        print*, "Complete..."
        print*, ""
    end subroutine Simpson38


    subroutine Trapezoidal()
        real:: a, b, h, tol, maxIts, its, sum, oldSum, relEr
        integer:: cont
        logical:: goal

        call askForLimits(a, b, tol, maxIts)

        cont=1
        its=0
        goal=.false.
        oldSum=0
        relEr=0
        sum=0

        print*, "Starting ..."
        print*, ""
        call funcionIntegralHumanize()

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
                    print *,'Tolerance passed'
                    print*, 'Integral = ', sum
                    call outIntegral(sum, relEr, tol, .false.)
                end if
            end if
            its = its+1
            oldSum = sum
            sum = 0
            cont = cont+1
        end do

        if(.NOT.goal) then
            print *,'Max iterations passed.'
            print*, 'Integral = ', oldSum
            call outIntegral(oldSum, relEr, tol, .false.)
        end if

        call system('clear')
        print*, "Complete..."
        print*, ""
    end subroutine Trapezoidal

    subroutine TrapezoidalWithData()
        real:: sum, trap
        integer:: cont, n
        real, dimension(:), allocatable::x
        real, dimension(:), allocatable::y
    
        open(7, file = 'inputs/Points.txt')
          read(7, *) n
    
        allocate(x(n))
          allocate(y(n))

        read(7, *) x
        read(7, *) y

        close(7)

        cont=1
        sum=0
        trap=0

        print*, "Starting ..."
        print*, ""

        do i=1, n-1
            sum=sum+(y(i)+y(i+1))*((x(i+1)-x(i))/2)
        end do

        print*, 'Integral = ', sum
        call outIntegral(sum, 0.0, 0.0, .true.)

        call system('clear')
        print*, "Complete..."
        print*, ""
    end subroutine TrapezoidalWithData

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