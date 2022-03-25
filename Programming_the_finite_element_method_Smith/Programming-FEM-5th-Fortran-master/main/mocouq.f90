SUBROUTINE mocouq(psi,dsbar,theta,dq1,dq2,dq3)
!
! This subroutine forms the derivatives of a Mohr-Coulomb potential
! function with respect to the three invariants (psi in degrees).
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::psi,dsbar,theta
 REAL(iwp),INTENT(OUT)::dq1,dq2,dq3
 REAL(iwp)::psir,snth,snps,sq3,c1,csth,cs3th,tn3th,tnth,zero=0.0_iwp,     &
   pt49=0.49_iwp,pt5=0.5_iwp,one=1.0_iwp,d3=3.0_iwp,d4=4.0_iwp,           &
   d180=180.0_iwp
 psir=psi*d4*ATAN(one)/d180 
 snth=SIN(theta) 
 snps=SIN(psir)
 sq3=SQRT(d3)  
 dq1=snps
 if(ABS(snth).GT.pt49)THEN
   c1=one
   IF(snth.LT.zero)c1=-one
   dq2=(sq3*pt5-c1*snps*pt5/sq3)*sq3*pt5/dsbar 
   dq3=zero
 ELSE
   csth=COS(theta)
   cs3th=COS(d3*theta)
   tn3th=TAN(d3*theta)
   tnth=snth/csth
   dq2=sq3*csth/dsbar*((one+tnth*tn3th)+snps*(tn3th-tnth)/sq3)*pt5
   dq3=pt5*d3*(sq3*snth+snps*csth)/(cs3th*dsbar*dsbar)
 END IF
RETURN
END SUBROUTINE mocouq
