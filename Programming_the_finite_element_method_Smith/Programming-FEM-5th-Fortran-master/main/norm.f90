FUNCTION norm(x)RESULT(l2n)
!
! THis function calculates the l2 norm of vector x
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::x(:)
 REAL(iwp)::l2n
 l2n=SQRT(SUM(x**2))
RETURN
END FUNCTION norm









