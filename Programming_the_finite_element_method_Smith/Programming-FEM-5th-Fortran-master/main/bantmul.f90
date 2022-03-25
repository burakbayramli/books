SUBROUTINE bantmul(kb,loads,ans)
! Used in p810
! This subroutine multiplies an unsymmetrical band kb by the vector loads.
! Could be much improved for vector processors.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::kb(:,:),loads(0:)
 REAL(iwp),INTENT(OUT)::ans(0:)
 INTEGER::i,j,k,l,m,n,iw
 REAL(iwp)::x,zero=0.0_iwp
 n=SIZE(kb,1)
 l=SIZE(kb,2)
 iw=(l-1)/2
 DO i=1,n
   x=zero
   k=iw+2
   DO j=1,l
     k=k-1
     m=i-k+1
     IF(m<=n.AND.m>=1)x=x+kb(i,j)*loads(m)
   END DO
   ans(i)=x
 END DO
RETURN
END SUBROUTINE bantmul 
