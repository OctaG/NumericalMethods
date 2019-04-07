MODULE FalsePositionMethod
use modulo_f

CONTAINS

  SUBROUTINE FalsePosition()
    real:: a, b, c, fa, fb, fc, cAnterior, tolerancia
    integer:: iter, max
    logical:: isInterval

    iter = 0

    call askForTwoInputs(a, b, isInterval)

    IF(isInterval) THEN
      call askForStopValues(tolerancia, max)
      !Saves the function evaluated in a to be used later
      fa = funcion(a)
      fb = funcion(b)
      
      print*, "Valid Interval. Operations may proceed"
      DO WHILE (iter < max)
      iter = iter + 1
      cAnterior = c
       c = b - (((fb)*(a-b))/(fa-fb))
      fc = funcion(c)
      error_relativo = calcularErrorRelativo(c, cAnterior)
        IF(error_relativo <= tolerancia) THEN
          call output(c, iter)
          exit
        ELSE IF(fa * fc > 0) THEN
          a = c
          !This minimizes function evaluation
          fa = fc
        ELSE
          b = c
          fb = fc
        END IF
       END DO
      IF(iter == max) THEN
        print*, "Maximum number of operations reached"
        print*,"Approximated value found: ", value
      END IF
    END IF
  END SUBROUTINE FalsePosition
END MODULE FalsePositionMethod
