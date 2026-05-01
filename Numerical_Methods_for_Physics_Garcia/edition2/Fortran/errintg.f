      real*8 function errintg( x, param )
      real*8 x, param(*)
! Error function integrand
! Inputs
!    x            Value where integrand is evaluated
!    param        Parameter list (not used)
! Output
!    errintg      Integrand of the error function

      errintg = exp(-x*x)
      return
      end
