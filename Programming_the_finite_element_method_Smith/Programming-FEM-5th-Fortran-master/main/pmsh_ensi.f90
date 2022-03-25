SUBROUTINE pmsh_ensi(argv,nlen,step,loads)
!
! This subroutine outputs pressure in the Ensight gold format for 
! visualization in ParaView. ParaView also requires the output of subroutine
! mesh_ensi for the geometry.
!  
  IMPLICIT none

  INTEGER,PARAMETER             :: iwp=SELECTED_REAL_KIND(15)
  INTEGER,   INTENT(IN)         :: nlen,step
  INTEGER                       :: i
  REAL(iwp), INTENT(IN)         :: loads(:)
  CHARACTER(LEN=15), INTENT(IN) :: argv  
  CHARACTER(LEN=5)              :: ch

  WRITE(ch,'(I5.5)') step ! convert integer to string using internal file

  OPEN(17,FILE=argv(1:nlen)//'.ensi.PRESSURE-'//ch)

  WRITE(17,'(A)') "Alya Ensight Gold --- Scalar per-node variable file"
  WRITE(17,'(A/A/A)') "part", "      1","coordinates"

  DO i=1,UBOUND(loads,1)
    WRITE(17,'(E12.5)') loads(i)
  END DO

  CLOSE(17)

  RETURN

END SUBROUTINE pmsh_ensi

