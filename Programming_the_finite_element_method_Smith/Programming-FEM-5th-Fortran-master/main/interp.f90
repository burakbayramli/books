SUBROUTINE interp(k,dtim,rt,rl,al,nstep)
!
! This subroutine forms the load/time functions by interpolation.
! If dtim is not an exact multiple it stops one short.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::dtim,rt(:),rl(:)
 INTEGER,INTENT(IN)::k,nstep
 REAL(iwp),INTENT(IN OUT)::al(:,:)
 INTEGER::np,i,j
 REAL(iwp)::t,val 
 np=SIZE(rt)
 al(1,k)=rl(1)
 t=rt(1)
 DO j=2,nstep
   t=t+dtim
   DO i=2,np
     IF(t.LE.rt(i))THEN
       val=rl(i-1)+((t-rt(i-1))/(rt(i)-rt(i-1)))*(rl(i)-rl(i-1))
       EXIT
     END IF
   END DO
   al(j,k)=val
 END DO
RETURN
END SUBROUTINE interp
