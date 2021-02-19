MODULE CTS130
! Define Cash Test Set, problems 1-30.
  
! Declare global parameters 
  INTEGER :: TP
  DOUBLE PRECISION :: EPS
  
  INTEGER, PARAMETER :: NODE=2,LEFTBC=1,RIGHTBC=NODE-LEFTBC 
  DOUBLE PRECISION, PARAMETER :: PI=3.141592653589793D0 
  
CONTAINS

  SUBROUTINE PROBLEM_DEFN(A,B,EPS0,EPSF)
    DOUBLE PRECISION :: A,B,EPS0,EPSF
    SELECT CASE (TP)
      CASE (1)
        A = 0D0
        B = 1D0
        EPSF = 0.001D0
        EPS0 = EPSF
      CASE (2)
        A = 0D0
        B = 1D0
        EPSF = 0.01D0
        EPS0 = EPSF
      CASE (3)
        A = -1D0
        B = +1D0
        EPSF = 0.0001D0
        EPS0 = EPSF
      CASE (4)
        A = -1D0
        B = +1D0
        EPSF = 0.01D0
        EPS0 = EPSF
      CASE (5)
        A = -1D0
        B = +1D0
        EPSF = 0.0001D0
        EPS0 = EPSF
      CASE (6)
        A = -1D0
        B = +1D0
        EPSF = 0.001D0
        EPS0 = EPSF
      CASE (7)
        A = -1D0
        B = +1D0
        EPSF = 0.0001D0
        EPS0 = EPSF
      CASE (8)
        A = 0D0
        B = 1D0
        EPS0 = 0.2D0
        EPSF = 0.01D0
        EPS0 = EPSF
      CASE (9)
        A = -1D0
        B = +1D0
        EPSF = 0.01D0
        EPS0 = EPSF
      CASE (10)
        A = -1D0
        B = +1D0
        EPSF = 0.01D0
        EPS0 = EPSF
      CASE (11)
        A = -1D0
        B = +1D0
        EPSF = 0.0001D0   
        EPS0 = EPSF  
      CASE (12)
        A = -1D0
        B = +1D0
        EPSF = 0.0001D0
        EPS0 = EPSF
      CASE (13)
        A = -1D0
        B = +1D0
        EPSF = 0.0001D0  
        EPS0 = EPSF    
      CASE (14)
        A = -1D0
        B = +1D0
        EPSF = 0.0001D0  
        EPS0 = EPSF   
      CASE (15)
        A = -1D0
        B = +1D0
        EPSF = 0.003D0 
        EPS0 = EPSF 
      CASE (16)
        A = 0D0
        B = 1D0
        EPSF = 0.11D0  
        EPS0 = EPSF     
      CASE (17)
        A = -0.1D0
        B = +0.1D0
        EPSF = 0.0001D0
        EPS0 = EPSF
      CASE (18)
        A = 0D0
        B = 1D0
        EPSF = 0.01D0
        EPS0 = EPSF
      CASE (19)
        A = 0D0
        B = 1D0
        EPSF = 0.005D0
        EPS0 = EPSF
      CASE (20)
        A = 0D0
        B = 1D0
        EPSF = 0.06D0
        EPS0 = EPSF
      CASE (21)
        A = 0D0
        B = 1D0
        ! Test Set has EPSF = 0.01, but the plot appears to correspond to this value.
        EPSF = 0.0001D0  
        EPS0 = EPSF
      CASE (22)
        A = 0D0
        B = 1D0
        EPSF = 0.01D0
        EPS0 = EPSF
      CASE (23)
        A = 0D0
        B = 1D0
        EPSF = 1D0/9D0
        EPS0 = EPSF
      CASE (24)
        A = 0D0
        B = 1D0
        EPS0 = 0.03D0
        EPSF = 0.001D0
      CASE (25)
        A = 0D0
        B = 1D0
        EPSF = 0.001D0
        EPS0 = EPSF
      CASE (26)
        A = 0D0
        B = 1D0
        EPSF = 0.005D0
        EPS0 = EPSF
      CASE (27)
        A = 0D0
        B = 1D0
        EPSF = 0.001D0
        EPS0 = EPSF
      CASE (28)
        A = 0D0
        B = 1D0
        EPSF = 0.001D0   
        EPS0 = EPSF 
      CASE (29)
        A = 0D0
        B = 1D0
        EPSF = 0.005D0
        EPS0 = EPSF
      CASE (30)
        A = 0D0
        B = 1D0
        EPS0 = 0.1D0
        EPSF = 0.005D0  
      CASE (31:32)
        PRINT *,'To solve a problem in the range 31-32, you must '
        PRINT *,'USE CTS3132 instead of USE CTS130.'
        STOP
      CASE DEFAULT
        PRINT *,'Test problem not recognized.'
        STOP
    END SELECT
  END SUBROUTINE PROBLEM_DEFN

  SUBROUTINE FSUB(X,Y,F)
	DOUBLE PRECISION :: X,Y(NODE),F(NODE),MU
    F(1) = Y(2)
    SELECT CASE (TP)
      CASE (1)
        F(2) = Y(1)/EPS
      CASE (2)
        F(2) = Y(2)/EPS
      CASE (3)
        F(2) = ( -(2D0 + COS(PI*X))*Y(2) + Y(1) - (1D0 + EPS*PI**2)*COS(PI*X)&
                 -(2D0 + COS(PI*X))*PI*SIN(PI*X) )/EPS
      CASE (4)
        F(2) = ( -Y(2) + (1D0 + EPS)*Y(1) )/EPS
      CASE (5)
        F(2) = (  X*Y(2) + Y(1) - (1D0 + EPS*PI**2)*COS(PI*X) &
                 +PI*X*SIN(PI*X) )/EPS  
      CASE (6)
        F(2) = ( -X*Y(2) - EPS*PI**2*COS(PI*X) -PI*X*SIN(PI*X) )/EPS            
      CASE (7)
        F(2) = ( -X*Y(2) + Y(1) - (1D0 + EPS*PI**2)*COS(PI*X) &
                 -PI*X*SIN(PI*X) )/EPS
      CASE (8)
        F(2) = -Y(2)/EPS
      CASE (9)
        F(2) = ( -4D0*X*Y(2) -2D0*Y(1) )/(EPS + X**2)
      CASE (10)
        F(2) = -X*Y(2)/EPS
      CASE (11)
        F(2) = (Y(1) - (EPS*PI**2 + 1D0)*COS(PI*X))/EPS        
      CASE (12)
        F(2) = (Y(1) - (EPS*PI**2 + 1D0)*COS(PI*X))/EPS
      CASE (13)
        F(2) = (Y(1) - (EPS*PI**2 + 1D0)*COS(PI*X))/EPS      
      CASE (14)
        F(2) = (Y(1) - (EPS*PI**2 + 1D0)*COS(PI*X))/EPS   
      CASE (15)
        F(2) = X*Y(1)/EPS  
      CASE (16)
        F(2) = -0.25D0*PI**2 *Y(1)/EPS**2         
      CASE (17)
        F(2) = -3D0*EPS*Y(1)/(EPS + X**2)**2
      CASE (18)
        F(2) = -Y(2)/EPS
      CASE (19)
        F(2) = ( -EXP(Y(1))*Y(2) + 0.5D0*PI*SIN(0.5D0*PI*X)*EXP(2D0*Y(1)) )/EPS
      CASE (20)
        F(2) = ( 1D0 - Y(2)**2 )/EPS
      CASE (21)
        F(2) = ( Y(1) + Y(1)**2 - EXP(-2D0*X/SQRT(EPS)) )/EPS
      CASE (22)
        F(2) = ( -Y(2) - Y(1)**2 )/EPS
      CASE (23)
        MU = 1D0/EPS
        F(2) = MU*SINH(MU*Y(1)) 
      CASE (24)
        AX = 1D0 + X**2
        DAX = 2D0*X
        GAMMA = 1.4D0
        F(2) = ( (0.5D0*(1D0+GAMMA) - EPS*DAX)*Y(1)*Y(2) - Y(2)/Y(1) - &
                 (DAX/AX)*(1D0 - 0.5D0*(GAMMA - 1D0)*Y(1)**2) )/(EPS*AX*Y(1))
      CASE (25:30)
        F(2) = ( -Y(1)*Y(2) + Y(1) )/EPS
      CASE DEFAULT
        PRINT *,'Test problem not implemented.'
        STOP        
    END SELECT
  END SUBROUTINE FSUB

  SUBROUTINE BCSUB(YA,YB,BCA,BCB)
	DOUBLE PRECISION :: YA(NODE),YB(NODE),BCA(LEFTBC),BCB(RIGHTBC)
    DOUBLE PRECISION :: END_VALUES(2),TEMP
    SELECT CASE (TP)
      CASE (1)
        END_VALUES = (/ 1D0,0D0 /)
      CASE (2)
        END_VALUES = (/ 1D0,0D0 /)
      CASE (3)
        END_VALUES = -1D0
      CASE (4)
        END_VALUES(1) = 1D0 + EXP(-2D0)
        END_VALUES(2) = 1D0 + EXP(-2D0*(1D0 + EPS)/EPS)
      CASE (5)
        END_VALUES = -1D0
      CASE (6)
        END_VALUES = (/ -2D0,0D0 /)
      CASE (7)
        END_VALUES = (/ -1D0,+1D0 /)
      CASE (8)
        END_VALUES = (/ 1D0,2D0 /)
      CASE (9)
        END_VALUES = 1D0/(1D0 + EPS)
      CASE (10)
        END_VALUES = (/ 0D0,2D0 /)
      CASE (11)
        END_VALUES = -1D0
      CASE (12)
        END_VALUES = (/ -1D0,0D0 /)
      CASE (13)
        END_VALUES = (/ 0D0,-1D0 /)
      CASE (14)
        END_VALUES = 0D0
      CASE (15)
        END_VALUES = 1D0
      CASE (16)
        END_VALUES = (/ 0D0,SIN(0.5D0*PI/EPS) /)
      CASE (17)
        TEMP = 0.1D0/SQRT(EPS + 0.01D0)
        END_VALUES = (/ -TEMP,TEMP /)
      CASE (18)
        END_VALUES = (/ 1D0,EXP(-1D0/EPS) /)
      CASE (19)
        END_VALUES = 0D0
      CASE (20)
        END_VALUES(1) = 1D0 + EPS*LOG(COSH(-0.745D0/EPS))
        END_VALUES(2) = 1D0 + EPS*LOG(COSH(+0.255D0/EPS))  
      CASE (21)
        END_VALUES = (/ 1D0,EXP(-1D0/SQRT(EPS)) /)   
      CASE (22)
        END_VALUES = (/ 0D0,0.5D0 /)  
      CASE (23)
        END_VALUES = (/ 0D0,1D0 /) 
      CASE (24)
        END_VALUES = (/ 0.9129D0,0.375D0 /)
      CASE (25)
        END_VALUES = (/ -1D0/3D0,+1D0/3D0 /)
      CASE (26)
        END_VALUES = (/ 1D0,(-1D0/3D0) /)
      CASE (27)
        END_VALUES = (/ 1D0,(+1D0/3D0) /)        
      CASE (28)
        END_VALUES = (/ 1D0,1.5D0 /)        
      CASE (29)
        END_VALUES = (/ 0D0,1.5D0 /)                
      CASE (30)
        END_VALUES = (/ (-7D0/6D0),1.5D0 /)                
      CASE DEFAULT
        PRINT *,'Test problem not implemented.'
        STOP        
    END SELECT
    BCA(1) = YA(1) - END_VALUES(1)
    BCB(1) = YB(1) - END_VALUES(2)
  END SUBROUTINE BCSUB

END MODULE CTS130
