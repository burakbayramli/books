SUBROUTINE formaa(vmfl,rmat,daatd)
!
! This subroutine modifies the dee matrix.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::vmfl(:),rmat(:,:)
 REAL(iwp),INTENT(OUT)::daatd(:,:)
 REAL(iwp)::flowt(1,4),flowa(4,1)   
 flowt(1,:)=vmfl 
 flowa(:,1)=vmfl
 daatd=MATMUL(MATMUL(MATMUL(rmat,flowa),flowt),rmat)
RETURN
END SUBROUTINE formaa
