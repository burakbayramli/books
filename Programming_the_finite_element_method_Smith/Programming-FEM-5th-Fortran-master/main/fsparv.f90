SUBROUTINE fsparv(kv,km,g,kdiag)
!
! This subroutine assembles element matrices into a symmetric skyline
! global matrix.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER,INTENT(IN)::g(:),kdiag(:)
 REAL(iwp),INTENT(IN)::km(:,:)
 REAL(iwp),INTENT(OUT)::kv(:) 
 INTEGER::i,idof,k,j,iw,ival
 idof=UBOUND(g,1)
 DO i=1,idof
   k=g(i)
   IF(k/=0)THEN
     DO j=1,idof
       IF(g(j)/=0)THEN
         iw=k-g(j)
         IF(iw>=0)THEN
           ival=kdiag(k)-iw
           kv(ival)=kv(ival)+km(i,j) 
         END IF
       END IF
     END DO
   END IF
 END DO
RETURN
END SUBROUTINE fsparv