SUBROUTINE formku(ku,km,g)
!
! This subroutine assembles element matrices into symmetrical
! global matrix (stored as an upper rectangle).
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::km(:,:)
 REAL(iwp),INTENT(OUT)::ku(:,:)
 INTEGER,INTENT(IN)::g(:) 
 INTEGER::i,j,icd,ndof
 ndof=UBOUND(km,1)
 DO i=1,ndof
   IF(g(i)/=0)THEN
     DO j=1,ndof
       IF(g(j)/=0)THEN
         icd=g(j)-g(i)+1
         IF(icd>=1)ku(g(i),icd)=ku(g(i),icd)+km(i,j)
       END IF
     END DO
   END IF
 END DO
RETURN
END SUBROUTINE formku
