SUBROUTINE solve_band(pb,work,loads)
!
! This subroutine performs Gaussian forward and back-substitution
! on the reduced unsymmetric band matrix pb.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::pb(:,:),work(:,:)
 REAL(iwp),INTENT(OUT)::loads(0:)
 INTEGER::iwp1,n,n1,i,iv,l,iq,iv1
 REAL(iwp)::s,pt5=0.5_iwp
 iwp1=(UBOUND(pb,2)-1)/2+1
 n=UBOUND(pb,1)
 iq=2*iwp1-1  
 n1=n-1
 DO iv=1,n1
   i=INT(work(iwp1,iv)+pt5)
   IF(i/=iv)THEN
     s=loads(iv)
     loads(iv)=loads(i)
     loads(i)=s
   END IF
   l=iv+iwp1-1
   IF(l>n)l=n
   iv1=iv+1
   DO i=iv1,l
     loads(i)=loads(i)-work(i-iv,iv)*loads(iv)
   END DO
 END DO
 loads(n)=loads(n)/pb(n,1)
 iv=n-1
 DO WHILE(iv/=0)
   s=loads(iv)
   l=iq
   IF(iv+l-1>n)l=n-iv+1
   DO i=2,l
     s=s-pb(iv,i)*loads(iv+i-1)
     loads(iv)=s/pb(iv,1)
   END DO
 iv=iv-1
 END DO
RETURN
END SUBROUTINE solve_band
