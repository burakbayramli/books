FUNCTION determinant(jac)RESULT(det)
!
! This function returns the determinant of a 1x1, 2x2 or 3x3
! Jacobian matrix.
!
 IMPLICIT NONE    
 !SELECTED_REAL_KIND: https://gcc.gnu.org/onlinedocs/gcc-7.1.0/gfortran/SELECTED_005fREAL_005fKIND.html#SELECTED_005fREAL_005fKIND
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::jac(:,:)
 REAL(iwp)::det
 INTEGER::it 
 it=UBOUND(jac,1)  
 SELECT CASE(it)
 CASE(1)
   det=1.0_iwp
 CASE(2)
   det=jac(1,1)*jac(2,2)-jac(1,2)*jac(2,1)
 CASE(3)
!rank = 3
   det=jac(1,1)*(jac(2,2)*jac(3,3)-jac(3,2)*jac(2,3))
   det=det-jac(1,2)*(jac(2,1)*jac(3,3)-jac(3,1)*jac(2,3))
   det=det+jac(1,3)*(jac(2,1)*jac(3,2)-jac(3,1)*jac(2,2))
 CASE DEFAULT
   WRITE(*,*)' wrong dimension for Jacobian matrix'
 END SELECT
RETURN
END FUNCTION determinant
