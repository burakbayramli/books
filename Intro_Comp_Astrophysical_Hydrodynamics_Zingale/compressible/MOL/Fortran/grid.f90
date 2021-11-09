module grid_module

  use datatypes_module

  implicit none

  ! for indexing the conservative state arrays
  integer, parameter :: URHO = 1
  integer, parameter :: UMX = 2
  integer, parameter :: UENER = 3

  ! for indexing the primitive variable arrays
  integer, parameter :: QRHO = 1
  integer, parameter :: QU = 2
  integer, parameter :: QP = 3

  integer, parameter :: NVAR = 3

  integer, parameter :: MOL_STAGES = 2

  real(dp_t), allocatable :: x(:)

  real(dp_t) :: dx

  integer :: ilo, ihi
  integer :: qx

  integer :: a_lo, a_hi

contains

  subroutine init_grid(ng)

    use runtime_params_module

    implicit none

    integer, intent(in) :: ng

    integer :: i

    qx = nx + 2*ng

    ilo = 0
    ihi = nx-1

    a_lo = -ng
    a_hi = nx+ng-1

    dx = (xmax - xmin)/nx

    allocate(x(a_lo:a_hi))

    do i = ilo-ng, ihi+ng
       x(i) = (dble(i) + 0.5_dp_t)*dx + xmin
    enddo

  end subroutine init_grid


  subroutine fill_ghost(U)
    ! just do zero-gradient

    implicit none

    integer :: n

    real(dp_t), intent(inout) :: U(a_lo:a_hi, NVAR)

    do n = 1, NVAR
       U(:ilo-1,n) = U(ilo,n)
    enddo

    do n = 1, NVAR
       U(ihi+1:,n) = U(ihi,n)
    enddo

  end subroutine fill_ghost

end module grid_module
