SUBROUTINE sparin_gauss(kv,kdiag)
!
! This subroutine performs Gaussian factorisation of a skyline matrix.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER,INTENT(IN)::kdiag(:)
 REAL(iwp),INTENT(OUT)::kv(:)
 REAL(iwp)::num,den,fac,zero=0.0_iwp
 INTEGER::n,ii,i,j,k,l,kk,l1,l2,l3
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
       kk=-1
       DO k=i,n
         kk=kk+1
         l1=kdiag(i+kk)-kk
         l2=l1-ii
         l3=kdiag(i+kk-1)
         IF(l2-l3>zero)kv(l1)=kv(l1)-fac*kv(l2)
       END DO 
     END IF
   END DO
 END DO
RETURN
END SUBROUTINE sparin_gauss
