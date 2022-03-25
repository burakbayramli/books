SUBROUTINE fmacat(vmfl,acat)
!
! This subroutine sets up an intermediate matrix acat.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::vmfl(:)
 REAL(iwp),INTENT(OUT)::acat(:,:)
 REAL(iwp)::temp(4,4),zero=0.0_iwp,pt5=0.5_iwp,one=1.0_iwp,three=3.0_iwp
 INTEGER::i,j
 temp=zero
 temp(1,1)=one
 temp(1,2)=-pt5
 temp(1,4)=-pt5
 temp(2,1)=-pt5
 temp(2,2)=one
 temp(2,4)=-pt5
 temp(3,3)=three
 temp(4,1)=-pt5
 temp(4,2)=-pt5
 temp(4,4)=one
 DO i=1,4
   DO j=1,4
     acat(i,j)=vmfl(i)*vmfl(j)
   END DO
 END DO
 acat=temp-acat
RETURN 
END SUBROUTINE fmacat
