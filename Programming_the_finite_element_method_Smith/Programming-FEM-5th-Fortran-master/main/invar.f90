SUBROUTINE invar(stress,sigm,dsbar,theta)
!
! This subroutine forms the stress invariants in 2- or 3-d.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::stress(:)
 REAL(iwp),INTENT(OUT),OPTIONAL::sigm,dsbar,theta
 REAL(iwp)::sx,sy,sz,txy,dx,dy,dz,xj3,sine,s1,s2,s3,s4,s5,s6,ds1,ds2,ds3, &
   d2,d3,sq3,zero=0.0_iwp,small=1.e-10_iwp,one=1.0_iwp,two=2.0_iwp,       &
   three=3.0_iwp,six=6.0_iwp,thpt5=13.5_iwp
 INTEGER::nst 
 nst=UBOUND(stress,1)
 SELECT CASE(nst)
 CASE(4)
   sx=stress(1)
   sy=stress(2)
   txy=stress(3)
   sz=stress(4)
   sigm=(sx+sy+sz)/three
   dsbar=SQRT((sx-sy)**2+(sy-sz)**2+(sz-sx)**2+six*txy**2)/SQRT(two)
   IF(dsbar<small)THEN
     theta=zero
   ELSE
     dx=(two*sx-sy-sz)/three
     dy=(two*sy-sz-sx)/three
     dz=(two*sz-sx-sy)/three
     xj3=dx*dy*dz-dz*txy**2
     sine=-thpt5*xj3/dsbar**3
     IF(sine>=one)sine=one
     IF(sine<-one)sine=-one
     theta=ASIN(sine)/three
   END IF
 CASE(6)
   sq3=SQRT(three)
   s1=stress(1)  
   s2=stress(2)
   s3=stress(3) 
   s4=stress(4)
   s5=stress(5)
   s6=stress(6)
   sigm=(s1+s2+s3)/three
   d2=((s1-s2)**2+(s2-s3)**2+(s3-s1)**2)/six+s4*s4+s5*s5+s6*s6
   ds1=s1-sigm 
   ds2=s2-sigm  
   ds3=s3-sigm
   d3=ds1*ds2*ds3-ds1*s5*s5-ds2*s6*s6-ds3*s4*s4+two*s4*s5*s6
   dsbar=sq3*SQRT(d2)
   IF(dsbar<small)THEN
     theta=zero
   ELSE
     sine=-three*sq3*d3/(two*SQRT(d2)**3)
     IF(sine>=one)sine=one 
     IF(sine<-one)sine=-one 
     theta=ASIN(sine)/three
   END IF
 CASE DEFAULT
   WRITE(*,*)"wrong size for nst in invar"
 END SELECT
RETURN
END SUBROUTINE invar



