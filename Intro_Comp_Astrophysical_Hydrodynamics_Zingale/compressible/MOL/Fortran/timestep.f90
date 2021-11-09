module timestep_module

  use datatypes_module

  implicit none

contains

  subroutine compute_dt(U, dt)

    use grid_module
    use runtime_params_module, only : gamma, cfl

    implicit none

    real(dp_t), intent(in) :: U(a_lo:a_hi, NVAR)
    real(dp_t), intent(out) :: dt

    real(dp_t) :: vel, p, cs
    integer :: i

    dt = 1.e33_dp_t

    do i = ilo, ihi
       vel = U(i, UMX)/U(i, URHO)
       p = (U(i, UENER) - 0.5_dp_t * U(i, URHO) * vel**2)*(gamma - 1.0_dp_t)

       cs = sqrt(gamma * p / U(i, URHO))
       dt = min(dt, cfl*dx/(abs(vel) + cs))
    enddo

  end subroutine compute_dt

end module timestep_module
