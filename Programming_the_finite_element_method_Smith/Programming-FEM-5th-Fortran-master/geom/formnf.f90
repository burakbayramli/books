SUBROUTINE formnf(nf)
! Used in many subroutines such as p41-p47, p51-p57, p61, p62, p65, p69, p610
! p91-p96, p101-p104, p111-p118
! This subroutine forms the nf matrix.
! nf  : nodal freedom matrix
 IMPLICIT NONE
 INTEGER,INTENT(IN OUT)::nf(:,:)
 INTEGER::i,j,m
 m=0
 DO j=1,UBOUND(nf,2)
   DO i=1,UBOUND(nf,1)
     IF(nf(i,j)/=0)THEN
       m=m+1
       nf(i,j)=m
     END IF
   END DO
 END DO
RETURN
END SUBROUTINE formnf 
