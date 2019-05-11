MODULE BisectionMethod
use modulo_f

CONTAINS

    SUBROUTINE Bisection()
        real:: a, b, c, fa, fb, fc, cAnterior, tolerancia
        integer:: iter, max !iter works as a conter
        logical:: isInterval

        iter = 0

        call askForTwoInputs(a, b, isInterval)

        IF(isInterval) THEN
            call askForStopValues(tolerancia, max)
            print*, "Starting ..."
            print*, ""
            !Saves the function evaluated in a to be used later
            fa = funcion(a)
            DO WHILE (iter < max)
                iter = iter + 1
                cAnterior = c
                c = (a + b) / 2
                fc = funcion(c)
                !error_relativo = calcularErrorRelativo(c, cAnterior)
                error = calcularErrorAbsoluto(c)
                IF(error <= tolerancia) THEN
                    call outputROOTFIND(c, iter, error, tolerancia, .true.)
                    exit
                ELSE IF(fa * fc > 0) THEN
                    a = c
                    !This minimizes function evaluation
                    fa = fc
                ELSE
                    b = c
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
    END SUBROUTINE Bisection

END MODULE BisectionMethod
