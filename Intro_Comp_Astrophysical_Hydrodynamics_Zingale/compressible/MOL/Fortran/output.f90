module io_module

  implicit none

contains

  subroutine output(t, istep, U)

    use grid_module
    use runtime_params_module, only : gamma

    implicit none

    real(dp_t), intent(in) :: t
    integer, intent(in) :: istep
    real(dp_t), intent(in) :: U(a_lo:a_hi, NVAR)

    integer :: lun

    real(dp_t) :: vel, p
    integer :: i
    character(len=4) :: step

    write(step, fmt="(i0.4)") istep

    open(newunit=lun, file="out_"//step//".dat", status="unknown")

    do i = ilo, ihi
       vel = U(i, UMX)/U(i, URHO)
       p = (U(i, UENER) - 0.5_dp_t * U(i, URHO) * vel**2)*(gamma - 1.0_dp_t)

       write(lun, *) x(i), U(i, URHO), vel, p
    enddo

    close(unit=lun)

  end subroutine output

end module io_module
