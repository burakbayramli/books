! Riemann solver for the LWR traffic model with an on-ramp or off-ramp

! q_t + (q(1-q))_x = D \delta(x)

! waves: 1
! equations: 1

! Conserved quantities:
!       1 q

! The strength of the on-ramp delta function and the index of the cell with
! the ramp (at the left edge) should be in the variables dramp, iramp in the
! common block cparam.

! See http://www.clawpack.org/riemann.html for a detailed explanation
! of the Riemann solver API.

subroutine rp1(maxm,num_eqn,num_waves,num_aux,num_ghost,num_cells, &
               ql,qr,auxl,auxr,fwave,s,amdq,apdq)

    implicit none

    ! Inputs
    integer, intent(in) :: maxm, num_eqn, num_waves, num_aux, num_ghost, num_cells
    double precision, intent(in), dimension(num_eqn, 1-num_ghost:maxm+num_ghost) :: ql,qr
    double precision, intent(in), dimension(num_aux, 1-num_ghost:maxm+num_ghost) :: auxl,auxr
    ! Outputs
    double precision, intent(out) :: s(num_waves,1-num_ghost:num_cells+num_ghost)
    double precision, intent(out) :: fwave(num_eqn,num_waves,1-num_ghost:num_cells+num_ghost)
    double precision, intent(out), dimension(num_eqn, 1-num_ghost:maxm+num_ghost) :: amdq,apdq
    ! Locals
    integer :: i, iramp
    double precision :: fim1, fi, sim1, si, f0
    double precision :: dramp

    common /cparam/ iramp, dramp

    do i=2-num_ghost, num_cells+num_ghost

        ! compute characteristic speed in each cell:
        sim1 = 1.d0 - 2.d0*qr(1,i-1)
        si   = 1.d0 - 2.d0*ql(1,i  )

        ! compute flux in each cell and flux difference:
        fim1 = qr(1,i-1)*(1.d0 - qr(1,i-1))
        fi   = ql(1,i  )*(1.d0 - ql(1,i  ))

        if (i.eq.iramp) then
            ! on-ramp here
            fwave(1,1,i) = fi - fim1 - dramp
            if (fim1+dramp .lt. 0.25d0 .and. si .le. 0.d0) then
                ! left-going
                s(1,i) = sim1
                amdq(1,i) = fwave(1,1,i)
                apdq(1,i) = 0.d0
            else if (sim1 .ge. 0.d0 .and. si .gt. 0.d0) then
                ! right-going
                s(1,i) = si
                amdq(1,i) = 0.d0
                apdq(1,i) = fwave(1,1,i)
            else if (sim1+dramp .gt. 0.25d0 .and. si .gt. 0.d0) then ! Should sim1 be fim1 here?
                ! transonic rarefaction
                s(1,i) = 0.5d0*(sim1 + si)

                ! entropy fix
                f0 = 0.25d0
                ! split fwave between amdq and apdq:
                amdq(1,i) = f0 - fim1 - dramp
                apdq(1,i) = fi - f0
            else
                ! transonic shock
                s(1,i) = 0.5d0*(sim1 + si)
                if (fi-fim1 .lt. 0.d0) then
                    amdq(1,i) = fwave(1,1,i)
                    apdq(1,i) = 0.d0
                else if (fi-fim1 .gt. 0.d0) then
                    amdq(1,i) = 0.d0
                    apdq(1,i) = fwave(1,1,i)
                else
                    amdq(1,i) = 0.5d0 * fwave(1,1,i)
                    apdq(1,i) = 0.5d0 * fwave(1,1,i)
                endif
            endif
        else
            ! No ramp here; use regular solver
            fwave(1,1,i) = fi - fim1

            if (sim1 .lt. 0.d0 .and. si .le. 0.d0) then
                ! left-going
                s(1,i) = sim1
                amdq(1,i) = fwave(1,1,i)
                apdq(1,i) = 0.d0
            else if (sim1 .ge. 0.d0 .and. si .gt. 0.d0) then
                ! right-going
                s(1,i) = si
                amdq(1,i) = 0.d0
                apdq(1,i) = fwave(1,1,i)
            else if (sim1 .lt. 0.d0 .and. si .gt. 0.d0) then
                ! transonic rarefaction
                s(1,i) = 0.5d0*(sim1 + si)

                ! entropy fix
                f0 = 0.25d0
                ! split fwave between amdq and apdq:
                amdq(1,i) = f0 - fim1
                apdq(1,i) = fi - f0
            else
                ! transonic shock
                s(1,i) = 0.5d0*(sim1 + si)
                if (fi-fim1 .lt. 0.d0) then
                    amdq(1,i) = fwave(1,1,i)
                    apdq(1,i) = 0.d0
                else if (fi-fim1 .gt. 0.d0) then
                    amdq(1,i) = 0.d0
                    apdq(1,i) = fwave(1,1,i)
                else
                    amdq(1,i) = 0.5d0 * fwave(1,1,i)
                    apdq(1,i) = 0.5d0 * fwave(1,1,i)
                endif
            endif
        endif
    end do
end subroutine rp1
