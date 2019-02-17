MODULE modulo_f
  implicit none

  CONTAINS

    !This is the function to which the numerical methods will be applied
    FUNCTION funcion(x)
      real:: funcion !Knows that a real will be returned
      real:: x
      funcion = 5+2*x-3*x**2+4*x**3
    END function funcion

    !This should be the derivative of the function above
    FUNCTION funcionDerivada(x)
      real:: funcionDerivada
      real:: x
      funcionDerivada = 2-6*x+12*x**2
    END function funcionDerivada

    FUNCTION calcularErrorRelativo(xNew, xOld)
      real:: calcularErrorRelativo
      real:: xNew, xOld
      calcularErrorRelativo = (ABS((xNew - xOld) / xNew)) * 100
    END function calcularErrorRelativo

    !Used in open methods
    SUBROUTINE askForOneInput(x1)
      real:: x1
      print*, "Give me x1: "
      read*, x1
   END SUBROUTINE askForOneInput

   !Used in bracketed methods
   SUBROUTINE askForTwoInputs(a, b, isInterval)
     integer:: option
     real:: a, b
     logical:: isInterval

     isInterval = .false.

     !Loops until there is at leats one root in the interval given
     DO WHILE( isInterval .neqv. .true.)
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
 
   SUBROUTINE askForStopValues(tolerancia, max)
     real:: tolerancia
     integer:: max
     print*, "Give me the input relative tolerance: "
     read*, tolerancia
     print*, "Give me the maximum number of iterations: "
     read*, max
   END SUBROUTINE askForStopValues

   !Result of all the methods
   SUBROUTINE output(c, iter)
     real:: c
     integer:: iter

     print*, "Value found: ", c
     print*, funcion(c)
     print*, iter
   END SUBROUTINE output

END MODULE modulo_f
