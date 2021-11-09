module runtime_params_module

  use datatypes_module

  implicit none

  real(dp_t) :: rho_l, rho_r
  real(dp_t) :: u_l, u_r
  real(dp_t) :: p_l, p_r
  real(dp_t) :: gamma
  real(dp_t) :: xmin, xmax
  real(dp_t) :: tmax, cfl
  integer :: nx

  character (len=32), save :: infile = ""

  namelist /params/ rho_l, rho_r, u_l, u_r, p_l, p_r, gamma, &
                    xmin, xmax, tmax, nx, cfl

contains

  subroutine get_runtime_parameters()

    implicit none

    integer :: lun

    ! set defaults
    rho_l = 1.0_dp_t
    rho_r = 0.1_dp_t

    u_l = 0.0_dp_t
    u_r = 0.0_dp_t

    p_l = 1.0_dp_t
    p_r = 0.125_dp_t

    gamma = 1.4_dp_t

    xmin = 0.0_dp_t
    xmax = 1.0_dp_t

    tmax = 0.2_dp_t

    nx = 64

    cfl = 0.8

    if (command_argument_count() == 0) then
       print *, 'no inputs file specified -- using parameter defaults'

    else if (command_argument_count() > 1) then
       print *, 'ERROR: only one command line argument allowed'

    else
       call get_command_argument(1, infile)
       print *, 'reading parameters from ', trim(infile)

       open(newunit=lun, file=trim(infile), status="old", action="read")
       read(unit=lun, nml=params)
       close(unit=lun)

    endif

  end subroutine get_runtime_parameters

end module runtime_params_module
