SUBROUTINE comsub(bk,loads)
! performs the complete gaussian backsubstitution  : complex version
IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)  
COMPLEX(iwp),INTENT(IN)::bk(:);COMPLEX(iwp),INTENT(IN OUT)::loads(0:)
INTEGER::nkb,k,i,jn,jj,i1,n,iw; COMPLEX::sum
n = UBOUND(loads,1); iw = UBOUND(bk,1)/n - 1
loads(1)=loads(1)/bk(1)
   DO i=2,n
      sum=loads(i);i1=i-1 ; nkb=i-iw
      IF(nkb<=0)nkb=1
      DO k=nkb,i1
         jn=(i-k)*n+k;sum=sum-bk(jn)*loads(k)
      END DO
      loads(i)=sum/bk(i)
   END DO
   DO jj=2,n
      i=n-jj+1;sum=.0;i1=i+1;nkb=i+iw
      if(nkb-n>0)nkb=n
      DO k=i1,nkb
           jn=(k-i)*n+i  ; sum=sum+bk(jn)*loads(k)
      END DO
      loads(i)=loads(i)-sum/bk(i)
   END DO
RETURN
END SUBROUTINE comsub
