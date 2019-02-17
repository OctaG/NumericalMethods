MODULE NewtonMethod
  use modulo_f

  contains
    SUBROUTINE Newton()
      real:: x1, fx, fx_prime, tolerancia, value, error_relativo, value_ant
      integer:: iter, max
      iter = 0

      call askForOneInput(x1)
      call askForStopValues(tolerancia, max)

      DO WHILE (iter < max)
        iter = iter + 1
      
        fx = funcion(x1)
        fx_prime = funcionDerivada(x1)

        value_ant = x1
        value = x1 - (fx/fx_prime)

        error_relativo = calcularErrorRelativo(value, value_ant)

        IF(error_relativo <= tolerancia) THEN
          call output(value, iter)
          exit
        ELSE
          x1 = value
        END IF
      END DO
      IF(iter == max) THEN
        print*, "Maximum number of operations reached"
        print*,"Approximated value found: ", value
      END IF
    END SUBROUTINE Newton

END Module NewtonMethod
