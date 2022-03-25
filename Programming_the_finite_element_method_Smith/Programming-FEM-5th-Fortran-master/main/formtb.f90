SUBROUTINE formtb(pb,km,g)
!
! This subroutine assembles an unsymmetrical band matrix pb from
! element constituent matrices km.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::km(:,:)
 INTEGER,INTENT(IN)::g(:)
 REAL(iwp),INTENT(OUT)::pb(:,:)
 INTEGER::i,j,idof,icd,iw
 idof=SIZE(km,1)
 iw=(SIZE(pb,2)-1)/2
 DO i=1,idof
   IF(g(i)/=0)THEN
     DO j=1,idof
       IF(g(j)/=0)THEN
         icd=g(j)-g(i)+iw+1
         pb(g(i),icd)=pb(g(i),icd)+km(i,j)
       END IF
     END DO
   END IF
 END DO
RETURN
END SUBROUTINE formtb
