SUBROUTINE beam_gm(gm,ell)
! Used in p46
! This subroutine forms the beam geometric matrix for stability analysis.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::ell
 REAL(iwp),INTENT(OUT)::gm(:,:)
 REAL(iwp)::pt1=0.1_iwp,opt2=1.2_iwp,two=2.0_iwp,d15=15.0_iwp,d30=30.0_iwp
 gm(1,1)=opt2/ell
 gm(1,2)=pt1
 gm(2,1)=pt1
 gm(1,3)=-opt2/ell
 gm(3,1)=-opt2/ell
 gm(1,4)=pt1
 gm(4,1)=pt1
 gm(2,2)=two*ell/d15
 gm(2,3)=-pt1
 gm(3,2)=-pt1
 gm(2,4)=-ell/d30
 gm(4,2)=-ell/d30
 gm(3,3)=opt2/ell
 gm(3,4)=-pt1
 gm(4,3)=-pt1
 gm(4,4)=two*ell/d15
RETURN
END SUBROUTINE beam_gm
