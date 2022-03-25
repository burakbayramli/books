SUBROUTINE elmat(area,rho,emm)
!
! This subroutine forms the "analytical" lumped mass matrix for
! quadrilateral 4- or 8-node plane strain elements.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::area,rho  
 REAL(iwp),INTENT(OUT)::emm(:,:)
 REAL(iwp)::zero=0.0_iwp,pt2=0.2_iwp,pt25=0.25_iwp
 INTEGER::i,ndof
 ndof=UBOUND(emm,1)
 emm=zero
 SELECT CASE(ndof)
 CASE(8) 
   DO i=1,8
     emm(i,i)=pt25*area*rho
   END DO
 CASE(16)
   DO i=1,16
     emm(i,i)=pt2*area*rho
   END DO
   DO i=1,13,4
     emm(i,i)=pt25*emm(3,3)
   END DO
   DO i=2,14,4
     emm(i,i)=pt25*emm(3,3)
   END DO                     
 CASE DEFAULT
   WRITE(*,*)"Wrong number of nodes for rectangular element"
 END SELECT
RETURN
END SUBROUTINE elmat   

