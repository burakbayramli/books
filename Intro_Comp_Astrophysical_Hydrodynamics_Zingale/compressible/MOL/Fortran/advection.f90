module advection_module

  use riemann_module

  implicit none

contains

  subroutine get_advective_source(U, k)

    use grid_module
    use runtime_params_module

    implicit none

    real(dp_t), intent(in) :: U(a_lo:a_hi, NVAR)
    real(dp_t), intent(inout) :: k(a_lo:a_hi, NVAR)

    real(dp_t) :: q(a_lo:a_hi, NVAR), dq(a_lo:a_hi, NVAR)
    real(dp_t) :: flux(a_lo:a_hi, NVAR)

    integer :: i, n
    real(dp_t) :: dc, dl, dr, da, test
    real(dp_t) :: q_l(NVAR), q_r(NVAR)

    ! convert to primitive variables
    q(:, QRHO) = U(:, URHO)
    q(:, QU) = U(:, UMX)/q(:, QRHO)
    q(:, QP) = (U(:, UENER) - 0.5_dp_t * q(:, QRHO) * q(:, QU)**2)*(gamma - 1.0_dp_t)

    ! compute slopes
    do n = 1, NVAR
       do i = ilo-1, ihi+1
          dc = 0.5_dp_t * (q(i+1, n) - q(i-1, n))
          dl = q(i+1, n) - q(i, n)
          dr = q(i, n) - q(i-1, n)

          test = dl * dr

          if (test > 0.0_dp_t) then
             if (abs(dl) < abs(dr)) then
                da = dl
             else
                da = dr
             endif
          else
             da = 0.0
          endif

          dq(i, n) = da

       enddo
    enddo

    ! compute interface states -- this is a loop over interfaces
    do i = ilo, ihi+1

       do n = 1, NVAR
          q_l(n) = q(i-1, n) + 0.5_dp_t * dq(i-1, n)
          q_r(n) = q(i, n) - 0.5_dp_t * dq(i, n)
       enddo

       ! solve Riemann problem
       call riemann(gamma, &
                    q_l(QRHO), q_l(QU), q_l(QP), &
                    q_r(QRHO), q_r(QU), q_r(QP), &
                    flux(i, URHO), flux(i, UMX), flux(i, UENER))
    enddo

    ! compute the advective source
    do i = ilo, ihi
       do n = 1, NVAR
          k(i, n) = -(flux(i+1, n) - flux(i, n))/dx
       enddo
    enddo

  end subroutine get_advective_source

end module advection_module
