
c     FRICTION v3

c     skin friction and form drag estimation program

c     by W. Mason,   October 21, 1989
c     Department of Aerospace and Ocean Engineering
c     Virginia Tech, Blacksburg, VA 24061
c     mason@aoe.vt.edu

c     version 3: mod to include laminar and Re as input, Feb. 24, 1992
c                minor mods: Nov.  7, 1992
c                            Feb. 26, 1994
c                            Sep. 13, 1996
c                            Dec. 14, 2000 by Andy Ko
c                            Mar. 14, 2001 by Andy Ko
c    						   Jan. 28, 2008 (Torenbeek form factor adopted)
c     Mod notes:
c     1. All output from the screen is also written to the output file. Also,
C        a pause statement was included in the end to prevent command window
C        from closing when program terminates. - Andy Ko 12/14/00
C     2. User is now able to change the working directory. Default is Z:\
C        which is the network drive. This can be changed to c:\ or any other
C        directory if desired. Also, a title screen was added. - Andy Ko 3/14/01 

      USE DFPORT
      DIMENSION     TITLE(15),   COMP(15,4),  SWET(15),    REFL(15),
     1              FF(15),      TC(15),      ICODE(15),   xmetbl(200),
     2              CFI(15),     R(15),       CDFCAS(200), CFSW(15),
     3              CFSWFF(15),  TRANS(15),   RNCAS(200),  CDICAS(200),
     4              alttbl(200)

      character*4   TITLE,COMP
      character*15  datset,outputfile
	CHARACTER*50 newdir
      INTEGER*4 istatus

      A      = 8.33333E-08
      IWRIT  = 6
      IREAD  = 1
	IFILE  = 8

C  Title screen - Andy Ko 3/14/01

      WRITE (*,*) '****************************************************'
	WRITE (*,*) '     VT Aerospace Aircraft Design Software Series'
	WRITE (*,*) '****************************************************'
	WRITE (*,*)
      WRITE (*,*) 'FRICTION v3.1'
      WRITE (*,*)
      WRITE (*,*) 'skin friction and form drag estimation program'
      WRITE (*,*)
      WRITE (*,*) 'by W. Mason,   October 21, 1989'
      WRITE (*,*) 'Department of Aerospace and Ocean Engineering'
      WRITE (*,*) 'Virginia Tech, Blacksburg, VA 24061'
      WRITE (*,*) 'whmason@vt.edu'
      WRITE (*,*) 
	WRITE (*,*) '****************************************************'
      WRITE (*,*)

C  Set the input/output directory. Default is Z:\ - Andy Ko 3/14/01
C	Change the above comment to make default the current directory-Lowe 2/1/2006
      
	istatus=enoent+1
C		WRITE(*,*) enoent	Commented by Lowe 2/1/2006
	IF (istatus .eq. enoent) THEN
	     WRITE(*,*) 'Cannot change directory to :\'
	     WRITE(*,*) 'Please log into the network'
	     Pause 'Press the ENTER key to exit'
	     STOP
      ENDIF
C		Added by Lowe 2/1/2006:
      WRITE (*,*) 'Current directory is the default'
	WRITE (*,*) 'If desired, please enter a new directory name. '
	WRITE (*,*) 'Type "default" for the current directory' 
	READ (*,*) newdir
	IF (newdir .eq. 'default') THEN
	   WRITE (*,*) 'Current directory selected'
      ELSE
	   istatus = CHDIR(newdir)
         IF (istatus .eq. enoent) THEN
            WRITE(*,*) 'The directory does not exist'
	      WRITE(*,*) 'The directory is set to the default'
         ELSEIF (istatus .eq. enotdir) THEN
            WRITE(*,*) 'Directory could not be changed'
	      WRITE(*,*) 'The directory is set to the default'
         ELSE
	      WRITE(*,*) 'directory has been changed to ',newdir
         ENDIF
      ENDIF

2     WRITE(6,5)
      READ (5,7) datset
      OPEN (unit = IREAD,file=datset,STATUS='old',ERR=500)
	WRITE(6,6)
	READ (5,7) outputfile

	OPEN (unit = IFILE,file=outputfile)

      READ (IREAD,10) TITLE
      READ (IREAD,15) SREF,SCALE,FNCOMP,Finmd
      ncomp  = fncomp
      inmd   = finmd
   
      ASCALE = SCALE*SCALE
      SREF   = SREF/ASCALE

      WRITE(iwrit,20) TITLE,SREF,SCALE,NCOMP,inmd
	WRITE(ifile,20) TITLE,SREF,SCALE,NCOMP,inmd
      WRITE(iwrit,25)
	WRITE(ifile,25)

c     read in the component geometry and lam/turb. switch

      sum = 0.0

      DO 40 I   = 1,NCOMP
      READ (IREAD,50) (COMP(I,J),J=1,4),
     1                 SWET(I),REFL(I),TC(I),FICODE,Ftrans
      ICODE(I)  = FICODE
      TRANS(I)  = Ftrans
      SWET(I)   = SWET(I)/ASCALE
      sum       = sum + swet(i)
      REFL(I)   = REFL(I)/SCALE
      FF(I)     = 1.00

cwhm	Torenbeek form factor adopted, Jan. 28. 2006
      IF(ICODE(I).EQ.0) then
                        FF(I) = 1.0 + 2.7*TC(I) + 100.*TC(I)**4
                        else
                        FF(I) = 1.0 + 1.5*TC(I)**1.5 + 7.*TC(I)**3
                        endif
      WRITE(iwrit,60) (COMP(I,J),J=1,4),SWET(I),REFL(I),TC(I),
     1                 ICODE(I),FF(I),trans(i)
	WRITE(ifile,60) (COMP(I,J),J=1,4),SWET(I),REFL(I),TC(I),
     1                 ICODE(I),FF(I),trans(i)
   40 CONTINUE

      write(iwrit,170) sum
	write(ifile,170) sum

      TWTAW     = 1.0
      XME       = 0.2
      alt       = 0.0
      JJ        =   0

c     read in the flight condition

   65 READ (IREAD,70, end = 200) XME,xinput
      if(xme .le. 0.0) go to 200

      if(inmd .eq. 0) then 
               alt = 1000.0*xinput
               kd  = 1
               call stdatm(ALT,t,p,rho,a,xmu,ts,rr,pp,rmr,qm,kd,kk)
               if (kk.ne.0) then 
			    write(iwrit,80)
	            write(ifile,80)
	         endif
               RN  = rmr*XME

                     else

               rn  = xinput*10.**6

                     endif

c     compute the skin friction

      JJ        = JJ + 1

      DO 100 I  = 1, NCOMP
      R(I)      = RN*REFL(I)
      Rec       = R(I)*trans(i)
      CFLAM     = 0.0
      CFTURBC   = 0.0

      call turbcf(R(I),XME,TWTAW,CFTURBL)

      if(trans(i) .gt. 0.0) then
                            call turbcf(Rec,XME,TWTAW,CFTURBC)
                            call lamcf (Rec,XME,TWTAW,CFLAM)
                            endif

      CFI(I)    = CFTURBL - trans(i)*(CFTURBC - CFLAM)

   90 CFSW(I)   = CFI(I)*SWET(I)
      CFSWFF(I) = CFI(I)*SWET(I)*FF(I)
  100 CONTINUE
 
      sum       = 0.
      sum2      = 0.
      sum3      = 0.
      if (inmd .eq. 0) then
	   WRITE (iwrit,120) RN,alt,xme
         WRITE (ifile,120) RN,alt,xme
	endif         
      if (inmd .eq. 1) then 
	   write (iwrit,122) RN,xme
         write (ifile,122) RN,xme
      endif
      WRITE (iwrit,125)
	WRITE (ifile,125)

      DO 150 I  = 1, NCOMP
      CDCOMP    = CFSWFF(I)/SREF
      sum       = sum  + CFSW(I)
      sum2      = sum2 + CFSWFF(I)
      sum3      = sum3 + CDCOMP
      WRITE(iwrit,160) (COMP(I,J),J=1,4),R(I),
     1                  CFI(I),CFSW(I),CFSWFF(I),CDCOMP
  150	WRITE(ifile,160) (COMP(I,J),J=1,4),R(I),
     1                  CFI(I),CFSW(I),CFSWFF(I),CDCOMP

      WRITE(iwrit,180) sum,sum2,sum3
	WRITE(ifile,180) sum,sum2,sum3

      CD         = SUM/SREF
      CDFORM     = (SUM2-SUM)/SREF
      WRITE (iwrit,190) CD,CDFORM
	WRITE (ifile,190) CD,CDFORM

      CDICAS(JJ) = CD
      CDFCAS(JJ) = CDFORM
      RNCAS(JJ)  = RN
      xmetbl(jj) = xme
      alttbl(jj) = alt

      GO TO 65
 
c     print out a summary of the results
 
  200 CONTINUE

      if (inmd .eq. 0) then 
	   WRITE (iwrit,210)
	   WRITE (ifile,210)
	endif
      if (inmd .eq. 1) then 
	   write (iwrit,212)
	   write (ifile,212)
	endif

      DO 400 J = 1,JJ
      CDSUM = CDICAS(J)+CDFCAS(J)
      if (inmd .eq. 0) then
                WRITE(iwrit,420) J,XMETBL(J),ALTTBL(J),RNCAS(J),
     1                           CDICAS(J),CDFCAS(J),CDSUM
	          WRITE(ifile,420) J,XMETBL(J),ALTTBL(J),RNCAS(J),
     1                           CDICAS(J),CDFCAS(J),CDSUM
                       else
                WRITE(iwrit,422) J,XMETBL(J),RNCAS(J),
     1                           CDICAS(J),CDFCAS(J),CDSUM
	          WRITE(ifile,422) J,XMETBL(J),RNCAS(J),
     1                           CDICAS(J),CDFCAS(J),CDSUM
                       endif
  400 continue

      WRITE(iwrit,430)
	WRITE(ifile,430)

	close(unit=ifile)

	Pause 'Press ENTER key to terminate'
 
    5 FORMAT(' Enter name of data set:')
    6 FORMAT(' Enter name of output file')
    7 FORMAT(a15)
   10 FORMAT(15A4)
   15 FORMAT(4F10.5,I2)    
   20 FORMAT(//5X,'FRICTION - Skin Friction and Form Drag Program'/
     1 5x,'W.H. Mason, Department of Aerospace and Ocean Engineering'/
     2 5x,'Virginia Tech, Blacksburg, VA 24061 email:whmason@vt.edu'/
     1 5x,'version:   Jan. 28, 2006' //
     1 5X,'CASE TITLE:',15A4//
     2 5X,'SREF =',F12.5,2X,'MODEL SCALE =',F7.3,2X,
     3 'NO. OF COMPONENTS =',I2/
     4 5x,'input mode = ',i2,2x,
     5 '(mode=0: input M,h;  mode=1: input M, Re/L)'/)
   25 FORMAT(3x,' COMPONENT TITLE',4X,'SWET (FT2)',2X,'REFL(FT) ',
     1 2X,'TC',2X,'ICODE',1X,'FRM FCTR',1x,'FTRANS')
   50 FORMAT (4A4,4X,7F10.5,I1)
   60 FORMAT(4X,4A4,1X,F11.4,1x,f8.3,F8.3,2X,I2,2X,F7.4,2x,f7.4)
   70 FORMAT(4f10.3)
   80 format(/4x,'bad return from stdatm - check data'/)
  120 FORMAT(/5X,'REYNOLDS NO./FT =',E9.3,3x,'Altitude = ',f9.2,
     1 3x,'XME =',f7.3/)
  122 FORMAT(/5X,'REYNOLDS NO./FT =',E9.3,3x,'XME =',f7.3/)
  125 FORMAT(7X,'COMPONENT',8X,'RN',6X,'  CF ',4X,
     1          'CF*SWET',3X,'CF*SWET*FF',3X,'CDCOMP')
  160 FORMAT(4X,4A4,E10.3,2X,F7.5,F9.5,2X,F10.5,F10.5)
  170 FORMAT(/7X,'TOTAL SWET = ',F12.4)
  180 FORMAT(35X,'SUM =',F8.5,2X,F10.5,f10.5)
  190 FORMAT (/4X,'FRICTION DRAG: CDF = ',F7.5,
     1   11X,'FORM DRAG: CDFORM = ',F7.5/)
  210 FORMAT(/5X,'SUMMARY'//6X,'J',4x,'XME',5x,'Altitude',5X,
     1 ' RE/FT ',5X,' CDF  ', 2X,' CDFORM',3X,'CDF+CDFORM')
  212 FORMAT(//5X,'SUMMARY'//6X,'J',3x,'XME',5x,
     1 ' RE/FT ',5X,' CDF  ', 2X,' CDFORM',3X,'CDF+CDFORM')
  420 FORMAT(5X,I2,1X,F7.3,2x,E10.3,3x,E10.3,3F10.5)
  422 FORMAT(5X,I2,1X,F7.3,2x,E10.3,3F10.5)
  430 FORMAT(/5X,'END OF CASE'//)

      STOP
C
C   Error in opening file statement - Andy Ko 3/14/01
  500 write(*,*) "File cannot be opened or does not exist. Please try ag
     &ain."
	GOTO 2

      END

      subroutine lamcf(Rex,Xme,TwTaw,CF)
c
c     flat plate laminar skin friction routine
c
c     uses the Eckert Refereence Temperatrure Method
c     as described in White, Viscous Fluid Flow, 1974 ed., pg 589-590.  
c
c     see Boundary Layer Analysis Methods in Aerocal Methods
c         Pak #4, pages 4-39 to 4-41
c 
c     coded in FORTRAN, Feb. 24, 1992
c     by W.H. Mason 
c
c     Input:
c                  Rex   - Reynolds number
c                  Xme   - Mach Number
c                  TwTaw - Tw/Taw, the wall temperature ratio
c
c     values of gamma, Pr, and Te are set in the program
c
c     Output:
c                CF    - total skin friction coefficient

      G     = 1.4
      Pr    = .72
      R     = sqrt(Pr)
      TE    = 390.
      TK    = 200.

      TwTe  = TwTaw*(1. + R*(g - 1.)/2.*Xme**2)
      TstTe = 0.5 + 0.039*Xme**2 + 0.5*TwTe

      Cstar = sqrt(TstTe)*(1. + Tk/Te)/(TstTe + Tk/Te)

      CF  = 2.*.664*sqrt(cstar)/sqrt(Rex)

      RETURN

      END
 
      subroutine turbcf(Rex,xme,TwTaw,CF)
c
c     flat plate turbulent skin friction routine
c     
c     uses the Van Driest II Method, NASA TN D-6945
c 
c     coded in FORTRAN, October 21, 1989, by W.H. Mason
c
c     Input:       Rex   - Reynolds number (based on length)
c                  Xme   - Mach Number
c                  TwTaw - Tw/Taw, the wall temperature ratio
c
c     values of gamma, recovery factor and Te are set in the program
c
c     Output:      CF    - total skin friction coefficient

      epsmax = 0.2e-8
      G      = 1.4
      r      = 0.88
      Te     = 222.0

      xm    = (G - 1.)/2*xme**2
      TawTe = 1. + r*xm
      F     = TwTaw*TawTe
      Tw    = F * Te
      A     = SQRT(r*xm/F)
      B     = (1. + r*xm - F)/F
      denom = SQRT(4.*A**2 + B**2)
      Alpha = (2.*A**2 - B)/denom
      Beta  = B/denom
      Fc    = ((1.0 + SQRT(F))/2.0)**2
      If ( xme .gt. 0.1) Fc = r*xm/(ASIN(Alpha) + ASIN(Beta))**2

      Xnum   = (1. + 122./Tw*10**(-5/Tw))
      Denom  = (1. + 122./Te*10**(-5/Te))
      Ftheta = SQRT(1./F)*(Xnum/Denom)
      Fx     = Ftheta/Fc

      RexBar = Fx * Rex

      Cfb    = 0.074/RexBar**0.20

      iter   = 0
   10 iter   = iter + 1
      if (iter .gt. 200) write(6,100) eps, epsmax
	if (iter .gt. 200) go to 20
      Cfo    = Cfb
      Xnum   = 0.242 - SQRT(Cfb)*ALOG10(RexBar*Cfb)
      Denom  = 0.121 + SQRT(Cfb)/ALOG(10.)
      Cfb    = Cfb*(1.0 + Xnum/Denom)
      eps    = abs(Cfb - Cfo)
      if (eps .gt. epsmax) go to 10
   20	continue
      CF     = Cfb/Fc

  100 format(/3x,'did not converge in turbcf'/)

       RETURN
       END

      subroutine stdatm(z,t,p,r,a,mu,ts,rr,pp,rm,qm,kd,kk)
c
c   *********** 1976 STANDARD ATMOSPHERE SUBROUTINE **********
c
c     Mason's BASIC program, converted to FORTRAN - Sept. 1, 1989
c
c     kd -   = 0 - metric units
c           <> 0 - English units
c
c     kk - 0 - good return
c          1 - error: altitude out of table,
c              do not use output
c
c     z  - input altitude, in feet or meters (depending on kd)
c
c     output:
c
c     t  - temp.
c     p  - pressure
c     r  - density
c     a  - speed of sound
c     mu - viscosity
c     
c     ts - t/t at sea level
c     rr - rho/rho at sea level
c     pp - p/p at sea level
c
c     rm - Reynolds number per Mach per unit of length
c     qm - dynamic pressure/Mach^2
c
      real k, h, mu, ml
      KK = 0
      K  = 34.163195
      C1 = 3.048E-04
      IF (KD .eq. 0) goto 1240
      TL = 518.67
      PL = 2116.22
      RL = .0023769
      AL = 1116.45
      ML = 3.7373E-07
      BT = 3.0450963E-08
      GOTO 1260
 1240 TL = 288.15
      PL = 101325
      RL = 1.225
      C1 = .001
      AL = 340.294
      ML = 1.7894E-05
      BT = 1.458E-06
 1260 H = C1 * Z / (1 + C1 * Z / 6356.766)
      IF (H .gt. 11.0) goto 1290
      T = 288.15 - 6.5 * H
      PP = (288.15 / T) ** ( - K / 6.5)
      GOTO 1420
 1290 IF (H .gt. 20.0) goto 1310
      T = 216.65
      PP = .22336 *  EXP ( - K * (H - 11) / 216.65)
      GOTO 1420
1310  IF (H .gt. 32.0) goto 1330
      T = 216.65 + (H - 20)
      PP = .054032 * (216.65 / T) ** K
      GOTO 1420
1330  IF (H .gt. 47.0) goto 1350
      T = 228.65 + 2.8 * (H - 32)
      PP = .0085666 * (228.65 / T) ** (K / 2.8)
      GOTO 1420
1350  IF( H .gt. 51.0) goto 1370
      T = 270.65
      PP = .0010945 *  EXP ( - K * (H - 47) / 270.65)
      GOTO 1420
1370  IF (H .gt. 71.) goto 1390
      T = 270.65 - 2.8 * (H - 51)
      PP = .00066063 * (270.65 / T) ** ( - K / 2.8)
      GOTO 1420
1390  IF (H .gt. 84.852) THEN 
                              kk = 1
                              write(6,200) H
                              return
                         END IF
      T = 214.65 - 2 * (H - 71)
      PP = 3.9046E-05 * (214.65 / T) ** ( - K / 2)
1420  RR = PP / (T / 288.15)
      MU = BT * T**1.5 / (T + 110.4)
      TS = T / 288.15
      A  = AL *  SQRT (TS)
      T  = TL * TS
      R  = RL * RR
      P  = PL * PP
      RM = R * A / MU
      QM = .7 * P
c
  200 format('   Out of Table in StdAtm- too high !'//
     1        4x,'H =',f12.3,'  > 84.852 km'/)
c
      return
      end

