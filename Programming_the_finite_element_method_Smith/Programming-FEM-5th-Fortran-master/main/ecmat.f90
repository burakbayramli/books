SUBROUTINE ecmat(ecm,fun,ndof,nodof)
!
! This subroutine forms the element consistent mass matrix.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::fun(:)
 REAL(iwp),INTENT(OUT)::ecm(:,:)
 INTEGER,INTENT(IN)::nodof,ndof
 INTEGER::nod,i,j
 REAL::nt(ndof,nodof),tn(nodof,ndof),zero=0.0_iwp
 nod=ndof/nodof
 nt=zero
 tn=zero
 DO i=1,nod 
   DO j=1,nodof
     nt((i-1)*nodof+j,j)=fun(i)
     tn(j,(i-1)*nodof+j)=fun(i)
   END DO
 END DO
 ecm=MATMUL(nt,tn)
RETURN
END SUBROUTINE ecmat
