SUBROUTINE spabac_gauss(kv,loads,kdiag)
!
! This subroutine performs Gaussian forwrad and back-substitution on a
! skyline matrix.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::kv(:)
 REAL(iwp),INTENT(IN OUT)::loads(0:)
 INTEGER,INTENT(IN)::kdiag(:)
 REAL(iwp)::num,den,fac,asum,zero=0.0_iwp
 INTEGER::i,j,l,n,ii,jj,l1,l2
 n=UBOUND(kdiag,1)
 DO j=1,n-1
   den=kv(kdiag(j))
   ii=0
   DO i=j+1,n
     ii=ii+1
     l=kdiag(i)-ii
     IF(l-kdiag(i-1)>zero)THEN
       num=kv(l)
       fac=num/den
       loads(i)=loads(i)-fac*loads(j)
     END IF
   END DO
 END DO
 loads(n)=loads(n)/kv(kdiag(n))
 DO i=n-1,1,-1
   jj=0
   asum=zero
   DO j=i+1,n
     jj=jj+1
     l1=kdiag(i+jj)-jj
     l2=kdiag(i+jj-1)
     IF(l1-l2>zero)asum=asum+kv(l1)*loads(j)
   END DO
   loads(i)=(loads(i)-asum)/kv(kdiag(i))
 END DO
RETURN
END SUBROUTINE spabac_gauss  















