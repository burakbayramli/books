SUBROUTINE fkdiag(kdiag,g)
!
! This subroutine computes the skyline profile.
!
 IMPLICIT NONE
 INTEGER,INTENT(IN)::g(:)
 INTEGER,INTENT(OUT)::kdiag(:)
 INTEGER::idof,i,iwp1,j,im,k
 idof=SIZE(g)
 DO i=1,idof
   iwp1=1
   IF(g(i)/=0)THEN
     DO j=1,idof
       IF(g(j)/=0)THEN
         im=g(i)-g(j)+1
         IF(im>iwp1)iwp1=im
       END IF
     END DO
     k=g(i)
     IF(iwp1>kdiag(k))kdiag(k)=iwp1
   END IF
 END DO
RETURN
END SUBROUTINE fkdiag
