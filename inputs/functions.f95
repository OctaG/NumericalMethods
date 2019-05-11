module functions
    implicit none
    contains

    !============================== Non linear Equation ==================================
    ! This tree functions will be used for Solution of Non Linear Equations
    ! (Bisection Method, False Position Method, Newton Raphson Method, Secant Method)

    !This is the function to which the numerical methods will be applied
    FUNCTION funcion(x)
        real:: funcion !Knows that a real value will be returned
        real:: x
        funcion = -0.0001237*x**5+0.0239*x**4-1.3246*x**3+21.2087*x**2-178.7351*x-227.5431
    END FUNCTION funcion

    ! This should be the derivative of the function above | Used for Newton Raphson Method and Secant
    FUNCTION funcionDerivada(x)
        real:: funcionDerivada
        real:: x
        funcionDerivada =-5*0.0001237*x**4+4*0.0239*x**3-3*1.3246*x**2+2*21.2087*x-178.7351
    END FUNCTION funcionDerivada

    ! This should print the function of above, in a way easy to read for humans
    SUBROUTINE funcionHumanize()
        print *, "Current function: ","4x^3 - 3x^2 + 2x + 5"
    END SUBROUTINE funcionHumanize

    ! =====================================================================================



    !============================== Integration ==================================
    ! This function will be used for Integrations Methods
    ! ()

    FUNCTION funcionIntegral(x)
        real:: funcionIntegral !Knows that a real will be returned
        real:: x
        real, parameter:: PI = 4.D0*DATAN(1.D0)
        funcionIntegral = (1/sqrt(2*PI)) * exp(-0.5*x**2)
    END FUNCTION funcionIntegral

    ! =====================================================================================



    !=============================== Ordinary diff eqts ===================================
    ! This function will be used for Ordinary diff eqts Methods
    ! ()
    function fdexy(x,y)
        real::fdexy
        real::x
        real::y
        !Se debe cambiar manualmente dependiendo el problema
        fdexy  = -2*(x**3) + 12*(x**2) -20*x +8.5
    end function
    !======================================================================================

end module functions
