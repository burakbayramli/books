SUBROUTINE mocouf(phi,c,sigm,dsbar,theta,f)
!
! This subroutine calculates the value of the yield function
! for a Mohr-Coulomb material (phi in degrees).
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::phi,c,sigm,dsbar,theta   
 REAL(iwp),INTENT(OUT)::f
 REAL(iwp)::phir,snph,csph,csth,snth,one=1.0_iwp,d3=3.0_iwp,d4=4.0_iwp,   &
   d180=180.0_iwp
 phir=phi*d4*ATAN(one)/d180
 snph=SIN(phir) 
 csph=COS(phir) 
 csth=COS(theta)
 snth=SIN(theta)
 f=snph*sigm+dsbar*(csth/SQRT(d3)-snth*snph/d3)-c*csph
RETURN
END SUBROUTINE mocouf
