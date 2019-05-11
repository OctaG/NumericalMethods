MODULE modulo_f
  use functions ! functions given by the user
  use files ! to read and write

  CONTAINS

   FUNCTION funcionIntegral(x)
      real:: funcionIntegral !Knows that a real will be returned
      real:: x
      real:: PI = 4.D0*DATAN(1.D0)
      funcionIntegral = (1/sqrt(2*PI)) * exp(-0.5*x**2)
    END FUNCTION funcionIntegral

  !============================================================================



subroutine writeFileToMatrix(n, a, b, x)
        character(len = 11) :: filename
        integer:: n
        real, dimension(:,:), allocatable :: a
        real, dimension(:), allocatable :: b
        real, dimension(:), allocatable :: x

        filename = "myData.txt"
        !print*, "Reading now from", filename
        open(1, file = 'inputs/'//filename)
        read(1, *) n

        allocate(a(n,n))
        allocate(b(n))
        allocate(x(n))

        read(1, *) a
        !Changes the format of the matrix
        a = transpose(a)

        read(1, *) b

        close (1, status = 'keep')
    end subroutine writeFileToMatrix


    subroutine readData(fileName, n, x, y)
        character(len = 11) :: fileName
        integer :: n
        real, dimension(:), allocatable :: x
        real, dimension(:), allocatable :: y

        open(3, file = 'inputs/'//filename)
        read(3, *) n

        allocate(x(n))
        allocate(y(n))

        read(3, *) x
        read(3, *) y
        close (3, status = 'keep')
    end subroutine readData


    subroutine writeResultsToFile(a, x, n)
        character(len = 12) :: filename
        real, dimension(:,:), allocatable :: a
        real, dimension(:), allocatable :: x
        integer :: i, n

        filename = "results.txt"
        open(2, file = filename)

        a = transpose(a)

        write(2, *) a
        write(2, *) "This are your results:"
        !Writes results as a list of variables
        do i = 1, n
        write(2, "(a2, i0, a3, f8.4)") "x", i, "= ", x(i)
        end do
        close (2, status = 'keep')
        !print*, "Results were written in", filename
    end subroutine writeResultsToFile

  !========================== Ord Diff Equations ==============================

  function fdexy(x,y)
  		real::fdexy
      real::x
      real::y
      !Se debe cambiar manualmente dependiendo el problema
      fdexy  = -2*(x**3) + 12*(x**2) -20*x +8.5
  end function


  subroutine in_data(numinterval, tol, a, b, fa, fixed, maxits)
      integer:: numinterval
      real:: tol
      real:: a
      real:: b
      real:: fa
      integer:: fixed
      integer:: maxits
      print*, "Give me number of intervals, tolerance, a, b, f(a), fixed, Max iterations"
      read*, numinterval, tol, a, b, fa, fixed, maxits
  end subroutine

  !============================================================================










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

END MODULE modulo_f
