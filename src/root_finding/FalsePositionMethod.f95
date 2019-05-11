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
        print*, "Starting ..."
        print*, ""
        !Saves the function evaluated in a to be used later
        fa = funcion(a)
        fb = funcion(b)
        DO WHILE (iter < max)
            iter = iter + 1
            cAnterior = c
            c = b - (((fb)*(a-b))/(fa-fb))
            fc = funcion(c)
            !error_relativo = calcularErrorRelativo(c, cAnterior)
            error_absoluto = calcularErrorAbsoluto(c)
            IF(error_absoluto <= tolerancia) THEN
                call outputROOTFIND(c, iter, error_absoluto, tolerancia, .true.)
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
            call outputROOTFIND(c, iter, error, tolerancia, .false.)
        END IF
    ELSE
        call system('clear')
        print*, "Does not work? There are more methods..."
        print*, ""

    END IF
    END SUBROUTINE FalsePosition
END MODULE FalsePositionMethod
