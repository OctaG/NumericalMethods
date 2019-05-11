MODULE NewtonMethod
    use modulo_f

    contains
    SUBROUTINE Newton()
        real:: x1, fx, fx_prime, tolerancia, value, value_ant
        integer:: iter, max
        logical:: derivativeIsOk = .false.

        iter = 0

        call askForOneInput(x1)
        call checkDerivative(x1, derivativeIsOk)

        IF(derivativeIsOk) THEN
            call askForStopValues(tolerancia, max)
            DO WHILE (iter < max)
                iter = iter + 1

                fx = funcion(x1)
                fx_prime = funcionDerivada(x1)

                value_ant = x1
                value = x1 - (fx/fx_prime)

                !error_relativo = calcularErrorRelativo(value, value_ant)
                error_absoluto = calcularErrorAbsoluto(value)

                IF(error_absoluto <= tolerancia) THEN
                    call outputROOTFIND(value, iter, error_absoluto, tolerancia, .true.)
                    exit
                ELSE
                    x1 = value
                END IF
            END DO
            IF(iter == max) THEN
                call outputROOTFIND(c, iter, error, tolerancia, .false.)
            END IF
        ELSE
            call system('clear')
            print*, "Does not work? There are more methods..."
            print*, ""
        END IF
    END SUBROUTINE Newton
END Module NewtonMethod
