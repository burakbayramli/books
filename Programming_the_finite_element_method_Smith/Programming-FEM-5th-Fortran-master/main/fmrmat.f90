SUBROUTINE fmrmat(vmfl,dsbar,dlam,dee,rmat)
!
! This subroutine forms the rmat matrix.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::vmfl(:),dsbar,dlam,dee(:,:)
 REAL(iwp),INTENT(OUT)::rmat(:,:)
 REAL(iwp)::acat(4,4),acatc(4,4),qmat(4,4),temp(4,4),con,zero=0.0_iwp,    &
   pt5=0.5_iwp,one=1.0_iwp,three=3.0_iwp
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
 acat=(temp-acat)/dsbar
 acatc=matmul(dee,acat)
 qmat=acatc*dlam
 DO i=1,4
   qmat(i,i)=qmat(i,i)+one
 END DO
 DO i=1,4
   con=qmat(i,i)
   qmat(i,i)=one
   qmat(i,:)=qmat(i,:)/con
   DO j=1,4
     IF(j/=i)THEN
       con=qmat(j,i)
       qmat(j,i)=zero
       qmat(j,:)=qmat(j,:)-qmat(i,:)*con
     END IF
   END DO
 END DO
 rmat=matmul(qmat,dee)
RETURN
END SUBROUTINE fmrmat
