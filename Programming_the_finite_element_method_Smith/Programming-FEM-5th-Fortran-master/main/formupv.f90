SUBROUTINE formupv(ke,c11,c12,c21,c23,c32)
!
! This subroutine forms the unsymmetrical stiffness matrix
! for the u-p-v version of the Navier-Stokes equations.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::c11(:,:),c21(:,:),c23(:,:),c32(:,:),c12(:,:)
 REAL(iwp),INTENT(OUT)::ke(:,:)  
 INTEGER::nod,nodf,ntot
 nod=UBOUND(c11,1)
 nodf=UBOUND(c21,1) 
 ntot=nod+nodf+nod
 ke(1:nod,1:nod)=c11  
 ke(1:nod,nod+1:nod+nodf)=c12
 ke(nod+1:nod+nodf,1:nod)=c21  
 ke(nod+1:nod+nodf,nod+nodf+1:ntot)=c23
 ke(nod+nodf+1:ntot,nod+1:nod+nodf)=c32 
 ke(nod+nodf+1:ntot,nod+nodf+1:ntot)=c11
RETURN
END SUBROUTINE formupv 
