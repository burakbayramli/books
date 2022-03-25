SUBROUTINE vmdpl(dee,stress,pl)
!
! This subroutine forms the plastic stress/strain matrix
! for a von-Mises material.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::stress(:),dee(:,:)  
 REAL(iwp),INTENT(OUT)::pl(:,:)
 REAL(iwp),ALLOCATABLE::dfds(:),ddfds(:)
 REAL(iwp)::t2,t6,t10,t14,t16,t17,t18,t19,t21,t22,t23,t25,t26,t30,sq2,sx, &
   sy,sz,txy,tyz,tzx,one=1.0_iwp,two=2.0_iwp,d4=4.0_iwp,d6=6.0_iwp
 REAL(iwp)::denom
 INTEGER::i,j,ih
 ih=SIZE(stress)
 ALLOCATE(dfds(ih),ddfds(ih))
 sq2=SQRT(two)
 SELECT CASE(ih)
 CASE(4)
   sx=stress(1)
   sy=stress(2)
   txy=stress(3) 
   sz=stress(4)   
   t2=sx**2
   t6=sy**2
   t10=sz**2
   t14=txy**2
   t17=SQRT(two*t2-two*sx*sy+two*t6-two*sy*sz+two*t10-two*sz*sx+d6*t14)
   t19=one/sq2/t17
   t21=two*sy
   t22=two*sz
   t26=two*sx
   dfds(1)=t19*(d4*sx-t21-t22)/two
   dfds(2)=t19*(-t26+d4*sy-t22)/two
   dfds(3)=t19*d6*txy
   dfds(4)=t19*(-t21+d4*sz-t26)/two
 CASE(6)
   sx=stress(1)
   sy=stress(2)
   sz=stress(3)   
   txy=stress(4) 
   tyz=stress(5) 
   tzx=stress(6) 
   t2=sx**2
   t6=sy**2
   t10=sz**2
   t14=txy**2
   t16=tyz**2
   t18=tzx**2
   t21=SQRT(two*t2-two*sx*sy+two*t6-two*sy*sz+two*t10-                    &
     two*sz*sx+d6*t14+d6*t16+d6*t18)
   t23=one/sq2/t21
   t25=two*sy
   t26=two*sz
   t30=two*sx
   dfds(1)=t23*(d4*sx-t25-t26)/two
   dfds(2)=t23*(-t30+d4*sy-t26)/two
   dfds(3)=t23*(-t25+d4*sz-t30)/two
   dfds(4)=t23*d6*txy
   dfds(5)=t23*d6*tyz
   dfds(6)=t23*d6*tzx
 END SELECT
 ddfds=MATMUL(dee,dfds) 
 denom=DOT_PRODUCT(ddfds,dfds)
 DO i=1,ih
   DO j=1,ih
     pl(i,j)=ddfds(i)*ddfds(j)/denom
   END DO
 END DO
 DEALLOCATE(dfds,ddfds)
RETURN
END SUBROUTINE vmdpl
