program euler

  use advection_module
  use grid_module
  use init_module
  use io_module
  use riemann_module
  use runtime_params_module
  use timestep_module

  implicit none

  real(dp_t), allocatable :: U_old(:,:)
  real(dp_t), allocatable :: U_new(:,:)

  ! for the MOL righthand sides
  real(dp_t), allocatable :: k(:,:,:)

  real(dp_t) :: t, dt

  integer :: n, istep
  integer, parameter :: ng = 2

  ! read in runtime parameters
  call get_runtime_parameters()

  ! create the grid
  call init_grid(ng)

  allocate(U_old(a_lo:a_hi, nvar))
  allocate(U_new(a_lo:a_hi, nvar))

  U_old(:,:) = 0.0_dp_t
  U_new(:,:) = 0.0_dp_t

  allocate(k(a_lo:a_hi, nvar, MOL_STAGES))

  k(:,:,:) = 0.0_dp_t

  ! initialize the problem
  call prob_init(U_old)

  ! evolution loop
  t = 0.0
  istep = 0

  call output(t, istep, U_old)

  do while (t < tmax)

     ! compute the timestep
     call compute_dt(U_old, dt)

     if (t + dt > tmax) then
        dt = tmax - t
     endif

     ! fill ghost cells
     call fill_ghost(U_old)

     ! get advective RHS
     call get_advective_source(U_old, k(:,:,1))

     ! predict state to next RK stage -- we'll temporarily store this in the
     ! "new slot"
     do n = 1, nvar
        U_new(:, n) = U_old(:, n) + 0.5_dp_t * dt * k(:, n, 1)
     enddo

     ! fill ghost cells
     call fill_ghost(U_new)

     ! get advective RHS
     call get_advective_source(U_new, k(:,:,2))

     ! do final update
     do n = 1, nvar
        U_new(:, n) = U_old(:, n) + dt * k(:, n, 2)
        !U_new(:, n) = U_old(:, n) + dt * k(:, n, 1)
     enddo

     t = t + dt
     istep = istep + 1

     U_old(:, :) = U_new(:, :)

     print *, istep, t, dt

  enddo

  call output(t, istep, U_new)

end program euler
