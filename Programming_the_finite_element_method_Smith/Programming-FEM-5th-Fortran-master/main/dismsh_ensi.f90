SUBROUTINE dismsh_ensi(argv,nlen,step,nf,loads)
!
! This subroutine outputs displacements in the Ensight gold format for
! visualization in ParaView. ParaView also requires the output of subroutine
! mesh_ensi for the geometry.
!
  IMPLICIT none

  INTEGER,PARAMETER             :: iwp=SELECTED_REAL_KIND(15)
  INTEGER,   INTENT(IN)         :: nlen,step,nf(:,:)
  INTEGER                       :: i,j
  REAL(iwp), INTENT(IN)         :: loads(:)
  CHARACTER(LEN=15), INTENT(IN) :: argv
  CHARACTER(LEN=5)              :: ch

  WRITE(ch,'(I5.5)') step ! convert integer to string using internal file

  OPEN(17,FILE=argv(1:nlen)//'.ensi.displ-'//ch)

  WRITE(17,'(A)') "Alya Ensight Gold --- Vector per-node variable file"
  WRITE(17,'(A/A/A)') "part", "      1","coordinates"

  DO i=1,UBOUND(nf,1)
    DO j=1,UBOUND(nf,2)
      WRITE(17,'(E12.5)') loads(nf(i,j))
    END DO
  END DO

  IF(UBOUND(nf,1)==2) THEN ! ensight requires zeros for the z-ordinate
    DO i=1,UBOUND(nf,2)
      WRITE(17,'(A)') " 0.00000E+00"
    END DO
  END IF

  CLOSE(17)

  RETURN

END SUBROUTINE dismsh_ensi
