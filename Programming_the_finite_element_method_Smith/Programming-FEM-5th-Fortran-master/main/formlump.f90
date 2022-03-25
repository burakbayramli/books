SUBROUTINE formlump(diag,emm,g)
!
! This subroutine forms the lumped global mass matrix as a vector diag.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::emm(:,:)  
 REAL(iwp),INTENT(OUT)::diag(0:)
 INTEGER,INTENT(IN)::g(:)
 INTEGER::i,ndof
 ndof=UBOUND(emm,1)
 DO i=1,ndof
   diag(g(i))=diag(g(i))+emm(i,i) 
 END DO
RETURN
END SUBROUTINE formlump   

