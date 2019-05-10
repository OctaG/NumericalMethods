MODULE modulo_f
  implicit none

  CONTAINS

  !============================== FUNCTION ====================================

    !This is the function to which the numerical methods will be applied
    FUNCTION funcion(x)
      real:: funcion !Knows that a real will be returned
      real:: x
      funcion = 5+2*x-3*x**2+4*x**3
    END FUNCTION funcion

    !This should be the derivative of the function above
    FUNCTION funcionDerivada(x)
      real:: funcionDerivada
      real:: x
      funcionDerivada = 2-6*x+12*x**2
    END FUNCTION funcionDerivada


   FUNCTION funcionIntegral(x)
      real:: funcionIntegral !Knows that a real will be returned
      real:: x
      funcionIntegral = 5+2*x-3*x**2+4*x**3
    END FUNCTION funcionIntegral

    ! This should print the function of above, in a way easy to read
    SUBROUTINE funcionHumanize()
      print *, "Current function: ","4x^3 - 3x^2 + 2x + 5"
    END SUBROUTINE funcionHumanize

  !============================================================================


  !================================= GENERAL ==================================

    !Used to calculate relative error
    FUNCTION calcularErrorRelativo(xNew, xOld)
      real:: calcularErrorRelativo
      real:: xNew, xOld
      IF(xNew /= 0) THEN ! not equal
        calcularErrorRelativo = (ABS((xNew - xOld) / xNew)) * 100
      END IF
    END function calcularErrorRelativo


    SUBROUTINE askForStopValues(tolerancia, max)
      real:: tolerancia
      integer:: max
      logical :: isValid = .false.

      ! loop until get positive values
      DO WHILE (.not. isValid)
        print*, "Give me the input relative tolerance: "
        read*, tolerancia
        isValid = tolerancia > 0
        IF (.not. isValid) THEN
          print *, "Tolerance have to be positive!"
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

    !Result of all the methods
    SUBROUTINE output(c, iter)
      real:: c
      integer:: iter

      print*, "Value found: ", c
      print*, "Value evaluated at function: ", funcion(c)
      print*, "Number of iterations: ", iter
    END SUBROUTINE output

  !============================================================================


  !============================= BRACKTED METHODS =============================

    !Used in bracketed methods
    SUBROUTINE askForTwoInputs(a, b, isInterval)
      integer:: option
      real:: a, b
      logical:: isInterval

      isInterval = .false.

      !Loops until there is at leats one root in the interval given
      DO WHILE( isInterval .neqv. .true.) !.neqv. => not equivalent
        print*, "Give me an interval a, b"
        read*, a, b
        IF(funcion(a) * funcion(b) > 0) THEN
          print*, "The interval given does not brackets a root"
          print*, "[Type 0 to give another interval or 1 to exit the method]"
          read*, option
          IF(option == 1) THEN
            exit
          END IF
        ELSE
          isInterval = .true.
        END IF
      END DO
    END SUBROUTINE askForTwoInputs

  !============================================================================


  !=============================== OPEN METHODS ================================

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
          print*, "[Type 0 to give another interval or 1 to exit the method]"
          read*, option
          IF(option == 1) THEN
            exit
          ELSE
            call askForOneInput(x1)
          END IF
        ELSE
          derivativeIsOk = .true.
        END IF
      END DO
    END SUBROUTINE checkDerivative

  !============================================================================

  !========================== READ & WRITE TO FILES ===========================

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

  !============================================================================

END MODULE modulo_f
