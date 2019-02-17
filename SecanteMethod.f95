MODULE SecanteMethod
  use modulo_f
  
  CONTAINS
    SUBROUTINE Secante()

      real:: x1, x2, fx1, fx2, tolerancia, value, error_relativo, value_ant
      integer:: iter, max
      iter = 0

      call askForOneInput(x1)
      x2 = x1 * 0.99

      call askForStopValues(tolerancia, max)

      DO WHILE (iter < max)

        iter = iter + 1
        fx1 = funcion(x1)
        fx2 = funcion(x2)
        value_ant = x1

        value = x2-(((fx2)*(x2-x1))/(fx2-fx1))

        error_relativo = calcularErrorRelativo(value, value_ant)

        IF(error_relativo <= tolerancia) THEN
          call output(value, iter)
          exit
        ELSE
          x1 = x2
          x2 = value
        END IF

      END DO

      IF(iter == max) THEN
        print*, "Maximum number of operations reached"
        print*,"Approximated value found: ", value
      END IF

    END SUBROUTINE Secante
END MODULE SecanteMethod
