MODULE modulo_f
  use functions ! functions given by the user
  use outs ! to read and write

  contains

!****************************************************************************************************
!******************************************* ROOT FINDING *******************************************
!****************************************************************************************************

  !========================= CALCULATE ERRORS =========================
    !Used to calculate relative error
  FUNCTION calcularErrorRelativo(xNew, xOld)
    real:: calcularErrorRelativo
    real:: xNew, xOld
    IF(xNew /= 0) THEN ! not equal
      calcularErrorRelativo = (ABS((xNew - xOld) / xNew)) * 100
    END IF
  END function calcularErrorRelativo

  !Used to calculate absolute error
  FUNCTION calcularErrorAbsoluto(x)
    real:: calcularErrorAbsoluto
    real:: x
      calcularErrorAbsoluto = ABS(funcion(x))
  END function calcularErrorAbsoluto
!====================================================================

!=================================== SHOW RESULTS ===================================
! Root finding

  SUBROUTINE outputROOTFIND(c, iter, error, tolerancia, before_max)
    real:: c, error, tolerancia
    integer:: iter
    logical :: before_max

    call system('clear')
    if(.not. before_max)then
      print*, "Maximum number of operations reached!!!"
    end if
    call funcionHumanize()
    print*, "Value found: ", c
    print*, "Value evaluated at function: ", funcion(c)
    print*, "Number of iterations: ", iter
    print*, "Error: ", error
    print*, "Tolerance ", tolerancia
    print*, ""
    call resultToFileROOTFIND(c, funcion(c), iter, error, tolerancia, before_max)
    call system('clear')
    print*, "Complete..."
    print*, ""

  END SUBROUTINE outputROOTFIND
!===================================================================================

!=============================== OPEN METHODS ================================
! Newton, Secant

  !Used in open methods
  SUBROUTINE askForOneInput(x1)
    real:: x1
    print*, "Give me x1: "
    read*, x1
  END SUBROUTINE askForOneInput

  SUBROUTINE checkDerivative(x1, derivativeIsOk)
    integer:: option
    real:: x1
    logical:: derivativeIsOk

    derivativeIsOk = .false.

    DO WHILE(derivativeIsOk .neqv. .true.)
      IF(funcionDerivada(x1) == 0) THEN
        print*, "The x1 you gave, evaluated in the derivative of the function is 0. Please give another value"
        print*, "[Type 1 to give another interval or 0 to exit the method]"
        read*, option
        IF(option == 0) THEN
          exit
        ELSE
          call askForOneInput(x1)
        END IF
      ELSE
        derivativeIsOk = .true.
      END IF
    END DO
  END SUBROUTINE checkDerivative

!================================================================================

!=============================== BRACKTED METHODS ===============================
!Bisection, False

  !Used in bracketed methods
  SUBROUTINE askForTwoInputs(a, b, isInterval)
    integer:: option
    real:: a, b
    logical:: isInterval

    isInterval = .false.

    !Loops until there is at leats one root in the interval given
    DO WHILE( isInterval .neqv. .true.) !.neqv. => not equivalent
      print*, "Give me an interval a, b (separated by space)"
      read*, a, b
      IF(funcion(a) * funcion(b) > 0) THEN
        print*, "The interval given does not brackets a root"
        print*, "[Type 1 to give another interval or 0 to exit the method]"
        read*, option
        IF(option == 0) THEN
          exit !form the loop
        END IF
      ELSE
        isInterval = .true.
        print*, "Valid Interval. Operations may proceed"
        print*, ""
      END IF
    END DO
  END SUBROUTINE askForTwoInputs

  SUBROUTINE askForStopValues(tolerancia, max)
    real:: tolerancia
    integer:: max
    logical :: isValid
    isValid = .false.
    tolerancia = -1

    ! loop until get positive values
    DO WHILE (.not. isValid)
      print*, "Give me the input relative tolerance: "
      read*, tolerancia
      isValid = 0 < tolerancia .and. tolerancia < 1
      IF (.not. isValid) THEN
        print *, "Tolerance should be a parcentage 0 < t < 1"
      END IF
    END DO

    isValid = .false.

    DO WHILE (.not. isValid)
      print*, "Give me the maximum number of iterations: "
      read*, max
      isValid = max > 0
      IF (.not. isValid) THEN
        print *, "Iterations have to be positive!"
      END IF
    END DO

  END SUBROUTINE askForStopValues

!==============================================================================
!****************************************************************************************************
!*************************************** END ROOT FINDING *******************************************
!****************************************************************************************************



!****************************************************************************************************
!********************************************** SOL EQ **********************************************
!****************************************************************************************************

  !======================== READ FILE ==========================
  subroutine writeFileToMatrix(n, a, b, x, copyOfA, copyOfB)
    integer:: n
    real, dimension(:,:), allocatable :: a, copyOfA
    real, dimension(:), allocatable :: b, copyOfB
    real, dimension(:), allocatable :: x

    !print*, "Reading now from", filename
    open(1, file = 'inputs/myData.txt')
    read(1, *) n

    allocate(a(n,n))
    allocate(b(n))
    allocate(x(n))
    allocate(copyOfA(n,n))
    allocate(copyOfB(n))

    read(1, *) a
    !Changes the format of the matrix
    a = transpose(a)

    read(1, *) b

    copyOfA = a
    copyOfB = b

    close (1, status = 'keep')
  end subroutine writeFileToMatrix
  !=============================================================

  !================= READ Right Hand Side file =================
  subroutine writeRHSToMatrix(b)
    real, dimension(:) :: b
    open(1, file = 'inputs/RHS.txt')
    read(1, *) b
    close (1, status = 'keep')
  end subroutine writeRHSToMatrix
  !=============================================================

  !======================== SHOW OUPUT ==========================
  subroutine writeResultsToFile(a, x, n, copyOfA, copyOfB)
      real, dimension(:,:), allocatable :: a, copyOfA
      real, dimension(:), allocatable :: x, copyOfB
      integer :: i, n

      a = transpose(a)

      !print*, a
      print*, "This are your results:"
      do i = 1, n
        print*, "x", i, "= ", x(i)
      end do
      call resultToFileSOLEQ(a,x,n,copyOfA,copyOfB)
      call system('clear')
      print*, "Complete..."
      print*, ""
  end subroutine writeResultsToFile
  !===============================================================

!****************************************************************************************************
!******************************************* END SOL EQ *********************************************
!****************************************************************************************************



!****************************************************************************************************
!******************************************* INTERPOLATION ******************************************
!****************************************************************************************************

  !================================ READ POINTS ======================================
  subroutine readPoints(x,y,n, points_file)
    character (len = 18) :: points_file
    real, dimension(:), allocatable::x
    real, dimension(:), allocatable::y
    integer:: n

    open(7, file = 'inputs/Points.txt')
  	read(7, *) n

	  allocate(x(n))
  	allocate(y(n))

    read(7, *) x
    read(7, *) y

    close(7)
  end subroutine readPoints
  !===================================================================================

  !================================ ASK FOR POINTS ======================================
  subroutine askForPoints(value,degree,point)
    integer:: degree, point
    real:: value, limit
    logical::isNotValid

    isNotValid=.true.

    print*, "Give me the point you want evaluated."
    read*, value

    print*, "Give me the degree of the polynomial"
    read*, degree

    DO WHILE (point < 1)
        print*, "Give me the point from which you want to start evaulating"
        read*, point
        IF(point < 1) THEN
            print*, "You can not give that value because the list of numbers starts at 1"
        END IF
    END DO

  DO WHILE (isNotValid)
    limit=point+degree
    IF((limit)>n) THEN
        print*, "The values for degree and initial point are not valid"
    print*, "Give me the degree of the polynomial"
      read*, degree
        point = 0
        DO WHILE (point < 1)
          print*, "Give me the point from which you want to start evaulating"
          read*, point
            IF(point < 1) THEN
                print*, "You can not give that value because the list of numbers starts at 1"
            END IF
        END DO
        limit = point+degree
    ELSE
        isNotValid=.false.
    END IF
    END DO
  end subroutine askForPoints

  subroutine askForPointsNEWTON(n, aux, x, y)
    real, dimension(:,:):: aux
    real, dimension(:) ::x
    real, dimension(:) ::y
    character (len = 23) :: file_name
    integer :: n, i, j, option, degree
    real :: value, sum, mtemp
    print*, "Type the file name where you want to save: "
    print*, "Up to 20 characters, please"
    read*, file_name
    open(1, file = 'results/'//file_name, action='write',position='append', status='unknown')
      do i = 1, n - 1, 1
        do j = 1, n -1, 1
          write(1, '(F8.3, A)',advance="no") aux(j,i), '|'
        end do
        write(1,*)
      end do
    close(1)
    print*,"All the coefficients are in the table now... "
    option = -1
    do while(option /= 0)
      print*, "Do you want to evaluate a point?"
      print*, "[Type 1 to yes or 0 to continue]"
      read*,option
      if(option == 1) then
        print *, "Give me the value of x in f(x) you want to evaluate"
        read*, value
        degree = -1
        do while(degree <= 0 .or. degree > (n - 1))
          print*, "Give me the degree of the polynomial (n -1) is the max, n = ", n
          read*, degree
          if (degree <= 0 .or. degree > (n - 1)) then
              print *, "Degree must be 0 < degree < n"
          end if
        end do
        sum = y(1)
        do i = 1, degree, 1
            mtemp = aux(i,1)
            do j = 1, i, 1
                !print *, "x", j
                mtemp = mtemp * (value - x(j))
            end do
            sum = sum + mtemp
        end do
        print*, "f(", value, ") = ", sum, "| with degree = ", degree
        call resultToFileINTERPOLNEWTON(file_name, value, sum, degree)
      else
        option = 0
      end if
    end do
    call system('clear')
    print*, "Complete..."
    print*, ""
  end subroutine askForPointsNEWTON
  !===================================================================================

!****************************************************************************************************
!**************************************** END INTERPOLATION *****************************************
!****************************************************************************************************


!****************************************************************************************************
!***************************************** REGRESSION ***********************************************
!****************************************************************************************************

! Read points is the same from Interpolation

  !===================================== ask for number of points ==============================
  subroutine askForNumberOfPoints(numberPoints, n)
    integer:: n, numberPoints
    print*, "Give me the number of points you want to use. They can't be less than 2 or more than ", n
    read*, numberPoints
    if(numberPoints>n) then
      numberPoints=n
      print*, "It was took number of points = ", n
    else if(numberPoints<2) then
      numberPoints=2
      print*, "It was took number of points = 2"
    end if
  end subroutine askForNumberOfPoints

  subroutine askForStartingPoint(startPoint, numberPoints, n)
    integer:: n, numberPoints, startPoint
    DO WHILE (startPoint < 1 .or. (n+1-startPoint)<numberPoints)
      print*, "Give me the point from which you want to start evaulating"
      read*, startPoint
      IF(startPoint < 1) THEN
          print*, "You can not give that value because the list of numbers starts at 1"
      ELSE IF((n+1-startPoint)<numberPoints) then
          print*, "You can not give that value because the remaining points are less than ", numberPoints
      END IF
    END DO
  end subroutine

  !=============================================================================================

  !=========================================== Show Results ====================================
  subroutine outRegression(sumSr, sumSt, rSquared, r, a0, a1, state, caso)
    character (len = 23) :: file_name
    real:: sumSr, sumSt, rSquared, r, pointToEvaluate, fpass
    logical::state
    integer::caso

    print*, "Type the file name where you want to save: "
    print*, "Up to 20 characters, please"
    read*, file_name
    open(1, file = 'results/'//file_name, action='write',position='append', status='unknown')
      write(1, *) "The equation for the linear regression is: ", a0, " + ", a1, "x"
      select case(caso)
        case (3)
          !print *, "Exponential"
          write(1, *) "The new exponential equation is: ", exp(a0), " * exp( ", a1, "x)"
        case (4)
          !print *, "Logarithmic"
          write(1,*) "The new power equation is: ", exp(a0), " * x^( ", a1, " )"
          write(1, *)
        case default
          print *, "Algo muy raro pasa aquí"
      end select
      write(1, *) "Sr = ", sumSr
      write(1, *) "St = ", sumSt
      write(1, *) "r^2 = ", r2
      write(1, *) "r = ", r
       write(1, *) ""
    close(1)

    print*, "The equation for the linear regression is: ", a0, " + ", a1, "x"
    print*, "Sr = ", sumSr
    print*, "St = ", sumSt
    print*, "r^2 = ", rSquared
    print*, "r = ", r

    select case(caso)
      case (3)
        !print *, "Exponential"
        print*, "The new exponential equation is: ", exp(a0), " * exp( ", a1, "x)"
      case (4)
        !print *, "Logarithmic"
        print*, "The new power equation is: ", exp(a0), " * x^( ", a1, " )"
      case default
        print *, "Algo muy raro pasa aquí"
    end select

    do while (state)
        print*, "Do you want to evaulate a point with the new exponential function? (Y=1/N=0, input a number)"
        read*, answer
        if(answer==1)then
            print*, "Give me the point you want to evaluate."
            read*, pointToEvaluate
            select case(caso)
              case (1)
                !print*, "Linear"
                fpass = (a0+(pointToEvaluate*a1))
              case (3)
                !print *, "Exponential"
                fpass = (exp(a0)*exp(pointToEvaluate*a1))
              case (4)
                !print *, "Logarithmic"
                fpass = (exp(a0)*pointToEvaluate**(a1))
              case default
                print *, "Algo muy raro pasa aquí"
            end select

            print*, "R(x)=", fpass
            call resultToFileREGRESSION(file_name, pointToEvaluate, fpass)
        else
            state=.false.
        end if
    end do
    call system('clear')
    print*, "Complete..."
    print*, ""
  end subroutine outRegression
  !=============================================================================================

!****************************************************************************************************
!**************************************** END REGRESSION ********************************************
!****************************************************************************************************


!****************************************************************************************************
!************************************** NUMERICAL INTEGRATION ***************************************
!****************************************************************************************************

  !========================== SHOW ==============================

  subroutine outIntegral(sum, error, tol, isData)
    real:: sum, error, tol
    character (len = 23) :: file_name
    logical:: isData
   
    print*, "Type the file name where you want to save: "
    print*, "Up to 20 characters, please"
    read*, file_name
    open(1, file = 'results/'//file_name, action='write',position='append', status='unknown')
      write(1,*) "Integral", sum
      if(.not. isData) then
        write(1,*) "Relative error", error
        write(1,*) "Tolerance: ", tol
      end if
      write(1,*) ""
    close(1)
  end subroutine outIntegral

  !============================================================================

  !
  subroutine askForLimits(a,b,tol,maxits)
    real:: a, b, h, tol, maxIts
    logical:: isValids

    print*, "Give me the lower limit (a)."
    read*, a

    print*, "Give me the upper limit (b)"
    read*, b

    tol = -1
    isValids = .false.
    DO WHILE (.not. isValids)
      print*, "Give me the input relative tolerance: "
      read*, tol
      isValids = 0 < tol .and. tol < 1
      IF (.not. isValids) THEN
        print *, "Tolerance should be a parcentage 0 < t < 1"
      END IF
    END DO

    print*, "Give me the maximum number of iterations"
    read*, maxIts
  end subroutine askForLimits

!****************************************************************************************************
!************************************** END NUMERICAL INTEGRATION ***********************************
!****************************************************************************************************


!****************************************************************************************************
!********************************* ORDINARY DIFFERENTIAL EQUATIONS **********************************
!****************************************************************************************************
  !========================== Ord Diff Equations ==============================
  subroutine in_data(numinterval, tol, a, b, fa, fixed, maxits)
    integer:: numinterval
    real:: tol
    real:: a
    real:: b
    real:: fa
    integer:: fixed
    integer:: maxits
    logical::validisimo
    print*, "Give me number of intervals"
    read*, numinterval
    tol = -1
    validisimo = .false.
    DO WHILE (.not. validisimo)
      print*, "Give me the tolerance: "
      read*, tol
      validisimo = 0 < tol .and. tol < 1
      IF (.not. validisimo) THEN
        print *, "Tolerance should be a parcentage 0 < t < 1"
      END IF
    END DO
    print*, "Give a "
    read*, a
    print*, "Give me b"
    read*, b
    print*, "Give me f(a)"
    read*, fa
    print*, "Is it fixed? [y = 1, n = 0]" 
    read*, fixed
    maxits = -1
    do while(maxits < 0)
      print*, "Give me the maximun number of iterations"
      read *, maxits
      if(maxits < 0) then
        print*, "Non valid"
      end if
    end do
    print*, "Starting ..."
    print*, ""
  end subroutine in_data

  
!============================================================================
!****************************************************************************************************
!********************************* END ORDINARY DIFFERENTIAL EQUATIONS ******************************
!****************************************************************************************************
END MODULE modulo_f
