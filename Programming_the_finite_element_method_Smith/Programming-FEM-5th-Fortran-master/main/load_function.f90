SUBROUTINE load_function(lf,dtim,al)
!
! This subroutine forms the increment of load at each
! calculation time step.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::lf(:,:),dtim
 REAL(iwp),INTENT(IN OUT)::al(:)
 REAL(iwp)::time,aold,anew
 INTEGER::nlfp,nincs,i,j
 nincs=SIZE(al)
 nlfp=UBOUND(lf,2)
 aold=lf(2,1)                        
 time=lf(1,1)
 DO i=1,nincs
   time=time+dtim
   DO j=1,nlfp
     IF(time<lf(1,j))THEN
       anew=lf(2,j-1)+                                                    &
         (time-lf(1,j-1))*(lf(2,j)-lf(2,j-1))/(lf(1,j)-lf(1,j-1))
       al(i)=anew-aold
       aold=anew
       EXIT
     END IF
   END DO
 END DO
RETURN
END SUBROUTINE load_function
