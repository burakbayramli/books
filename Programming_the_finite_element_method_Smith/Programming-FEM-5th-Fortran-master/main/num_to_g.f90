SUBROUTINE num_to_g(num,nf,g)
!
! This subroutine finds the g vector from num and nf.
!
 IMPLICIT NONE
 INTEGER,INTENT(IN)::num(:),nf(:,:)  
 INTEGER,INTENT(OUT)::g(:)
 INTEGER::i,k,nod,nodof 
 nod=UBOUND(num,1) 
 nodof=UBOUND(nf,1)
 DO i=1,nod
   k=i*nodof
   g(k-nodof+1:k)=nf(:,num(i))
 END DO
RETURN
END SUBROUTINE num_to_g   
