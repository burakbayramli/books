SUBROUTINE checon(loads,oldlds,tol,converged)
!
! This subroutine sets converged to .FALSE. if relative change in loads
! and oldlds is greater than tol and updates oldlds.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::loads(0:),tol
 REAL(iwp),INTENT(IN OUT)::oldlds(0:)
 LOGICAL,INTENT(OUT)::converged
 CONVERGED=.TRUE.
 CONVERGED=(MAXVAL(ABS(loads-oldlds))/MAXVAL(ABS(loads))<=tol)
 oldlds=loads
RETURN
END SUBROUTINE checon
