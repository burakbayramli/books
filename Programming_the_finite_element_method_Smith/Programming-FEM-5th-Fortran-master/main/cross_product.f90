SUBROUTINE cross_product(b,c,a)
!
! This subroutine forms the cross product of two vectors, a = b x c
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::b(:),c(:)
 REAL(iwp),INTENT(OUT)::a(:,:)
 INTEGER::ib,ic,i,j
 ib=SIZE(b)
 ic=SIZE(c)
 DO i=1,ib
   DO j=1,ic
     a(i,j)=b(i)*c(j)
   END DO
 END DO
RETURN
END SUBROUTINE cross_product   
