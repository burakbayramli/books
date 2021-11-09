module init_module

  use datatypes_module

  implicit none

contains

  subroutine prob_init(U)

    use grid_module
    use runtime_params_module

    implicit none

    real(dp_t) :: U(a_lo:a_hi, NVAR)

    integer :: i

    do i = ilo, ihi
       if (x(i) < 0.5_dp_t) then
          ! left half
          U(i,URHO) = rho_l
          U(i,UMX) = rho_l * u_l
          U(i,UENER) = p_l/(gamma - 1.0_dp_t) + 0.5_dp_t * rho_l * u_l**2

       else
          ! left half
          U(i,URHO) = rho_r
          U(i,UMX) = rho_r * u_r
          U(i,UENER) = p_r/(gamma - 1.0_dp_t) + 0.5_dp_t * rho_r * u_r**2

       endif

    enddo

  end subroutine prob_init

end module init_module
