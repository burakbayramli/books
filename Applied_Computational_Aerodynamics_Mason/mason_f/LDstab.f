c///////////////////////////////////////////////////////////////////////
c
c  Virginia Tech Department of Aerospace and Ocean Engineeirng
c
c  Program LDstab estimates Lateral/Directional Stability and Control
c  derivatives for transonic tranport class aircraft. It consists of 
c  a main program stabin that reads in the data, and a subroutine stab
c  that does the calculations. 
c
c  The program was written by Joel Grasmeyer, and is described in
c  VPI-AOE-254, which explains the input data required. The sign
c  conventions are also described there.
c
c  program stabin
c
c    This program reads the stability input file, and calls the stab
c  subroutine to calculate the maximum available yawing moment
c  coefficient.
c
c  See stab.f for variable definitions
c
c  Created by:  Joel Grasmeyer
c  Last Modified:  10/28/97
c
c  Mods:
c	- Modified for use in Aircraft design class by W.H. Mason, 3/30/2000
c	- Changed header, added pause at end, added an output file,
c	  L.T.Leifsson March 2004
c	- Added Cnbeta contribution from wing using DATCOM method,
c       this required adding two input variables: xbar and cbar,
c	  by L.T. Leifsson, March 2004.
c
c///////////////////////////////////////////////////////////////////////

      program stabin

      implicit none

      character*72 infile, outfile, header, title
      integer i, write_flag, unit_in, unit_outfile, new, nef
      real dihedral_wing, z_wing, dia_fuse, sref, hspan_vtail,
     & depth_fuse_vtail, c_vtail_root, c_vtail_tip, mach_eo, l_vtail,
     & sweep_wing_1_2, hspan_wing, cl, z_vtail,
     & length_fuse, rho_eo, a_eo, mu_eo, cn_avail, dia_nacelle,
     & ar_vtail_eff, dr_max, sh, sweep_vtail_1_4, xbar, cbar

c Get input and output filenames from command line

c      write(6,10)
	write(6,3000)
	write(6,3001)
	write(6,3002)
	write(6,3003)
	write(6,3004)
	write(6,3005)
      read(5,20) infile
	write(6,3006)
	read(5,20) outfile

c Read input that is common to both modes
      unit_in = 10
      open(unit_in,file=infile)
      read(unit_in,"(a72)") header
      read(unit_in,"(a72)") title
      read(unit_in,*) write_flag
      read(unit_in,*) dihedral_wing
      read(unit_in,*) z_wing
      read(unit_in,*) dia_fuse
      read(unit_in,*) sref
      read(unit_in,*) hspan_vtail
      read(unit_in,*) depth_fuse_vtail
      read(unit_in,*) c_vtail_root
      read(unit_in,*) c_vtail_tip
      read(unit_in,*) mach_eo
      read(unit_in,*) sweep_vtail_1_4
      read(unit_in,*) sweep_wing_1_2
      read(unit_in,*) hspan_wing
      read(unit_in,*) cl
      read(unit_in,*) z_vtail
      read(unit_in,*) l_vtail
      read(unit_in,*) length_fuse
      read(unit_in,*) new
      read(unit_in,*) nef
      read(unit_in,*) dia_nacelle
      read(unit_in,*) sh
      read(unit_in,*) rho_eo
      read(unit_in,*) a_eo
      read(unit_in,*) mu_eo
      read(unit_in,*) dr_max
	read(unit_in,*) xbar
	read(unit_in,*) cbar

c Close input file
      close(unit_in)
c Open output file and write header
	unit_outfile = 20
	open(unit_outfile,file=outfile)
	write(unit_outfile,3000)
	write(unit_outfile,3001)
	write(unit_outfile,3002)
	write(unit_outfile,3003)
	write(unit_outfile,3004)

c Echo input to output file

c Call stab subroutine
      call stab(outfile,title,write_flag,dihedral_wing,z_wing,
     & dia_fuse,sref,hspan_vtail,depth_fuse_vtail,c_vtail_root,
     & c_vtail_tip,mach_eo,sweep_vtail_1_4,sweep_wing_1_2,hspan_wing,
     & cl,z_vtail,l_vtail,length_fuse,new,nef,dia_nacelle,
     & sh,rho_eo,a_eo,mu_eo,dr_max,xbar,cbar,ar_vtail_eff,cn_avail)

c Pause before closing program
	PAUSE 'Press ENTER to close program'
c Write statements and formats  
   10 format(/2x, 'Lateral-Directional Stability and Control Derivative',
     1            ' Estimation'/2x,
     1            '-intended for transonic tranports, i.e., B747 -'//2x,
     1            'Joel Grasmeyer'/2x,
     2            'Aerospace and Ocean Engineering, Virginia Tech'/2x,
     3            'Blacksburg, VA 24061'//2x,
     4            'Aircraft Design Class Software'//2x,
     5            'Enter the name of input data file')
   20 format(a20)
 3000 format('  **************************************************'/
     1        '    VT Aerospace Aircraft Design Software Series'/
     2		'  **************************************************'/)
 3001 format('                   Program LDstab'/
     1		'  The program estimates Lateral/Directional Stability'/ 
     2		'  and Control derivatives for transonic tranport class'/
     3		'  aircraft.'/)
 3002 format('  Written by Joel Grasmeyer.'/
     1		'  This program modified by'/
	2		'    - W.H. Mason, March 2000,'/
     3		'    - L.T. Leifsson, March 2004.'/)
 3003 format('  The Department of Aerospace and Ocean Engineering,'/
     1		'  Virginia Tech, Blacksburg, VA 24061'/
     2		'  http://www.aoe.vt.edu, email: whmason@vt.edu'/)
 3004 format('  **************************************************'/)
 3005	format('  Enter input filename:')
 3006	format('  Enter output filename:')
      end ! End program stabin

c///////////////////////////////////////////////////////////////////////
c
c  subroutine stab
c
c    This subroutine calculates the maximum available yawing moment
c  coefficient.  Note that right rudder deflection is defined as
c  positive, and right aileron up, left aileron down is defined as
c  positive.  Both of these control deflections generate positive
c  moments about their respective axes.  This is the convention used
c  by Roskam.
c
c  Inputs
c
c  outfile      output filename
c  title        title of aircraft configuration
c  write_flag   write flag (0 = no output file, 1 = output file written)
c  dihedral_wing  wing dihedral angle (deg)
c  z_wing       distance from body centerline to quarter-chord point of
c                exposed wing root chord, positive for the quarter-chord
c                point below the body centerline (ft)
c  dia_fuse     fuselage diameter (ft)
c  sref         wing reference area (ft^2)
c  hspan_vtail  vertical tail span (ft)
c  depth_fuse_vtail  fuselage depth at the fuselage station of the
c                     quarter-chord of the vertical tail (ft)
c  c_vtail_root root chord of vertical tail
c  c_vtail_tip  tip chord of vertical tail
c  mach_eo      mach number
c  sweep_vtail_1_4  vertical tail quarter-chord sweep angle (deg)
c  sweep_wing_1_2  average wing half-chord sweep angle (deg)
c  hspan_wing   wing half-span (ft)
c  cl           lift coefficient
c  z_vtail      vertical distance from CG to AC of vertical tail (ft)
c  l_vtail      horizontal distance from CG to AC of vertical tail (ft)
c  length_fuse  fuselage length (ft)
c  new          number of engines on the wing
c  nef          number of engines on the fuselage
c  dia_nacelle  nacelle diameter (ft)
c  rho_eo       density at engine-out flight condition (slug/ft^3)
c  a_eo         speed of sound at engine-out flight condition (ft/s)
c  mu_eo        viscosity at engine-out flight condition (slug/(ft-s))
c  dr_max       maximum allowable steady-state rudder deflection (deg)
c  xbar		  longitudinal distance (positive rearward) from coordinate
c			  origin (usually the center of gravity) to the wing aero-
c			  dynamic center (ft), L.T. Leifsson March 2004.
c  cbar		  mean aerodynamic chord (ft), L.T. Leifsson March 2004.
c
c  Outputs
c
c  ar_vtail_eff  effective aspect ratio of vertical tail
c  cn_avail     maximum available yawing moment coefficient
c
c  Internal Variables
c
c  alpha        angle of attack (rad)
c  alpha_d      section lift effectiveness
c  alpha_d_cl   section flap effectiveness (from Figure 10.2)
c  ar           wing aspect ratio
c  ar_vtail     actual aspect ratio of vertical tail
c  beta         sideslip angle, positive from the right (rad)
c  beta_m       square root of (1 - mach_eo)**2
c  cf_c         ratio of flap chord to wing or tail chord
c  cl_alpha     lift-curve slope of entire aircraft (rad^-1)
c  cl_alpha_2d  2-dimensional lift-curve slope at MAC (rad^-1)
c  cl_alpha_vtail  original lift-curve slope of vertical tail (rad^-1)
c  cl_alpha_vtail_eff  effective lift-curve slope of vertical tail (rad^-1)
c  cl_beta      variation of rolling moment coefficient with sideslip angle
c  cl_beta_htail  horizontal tail contribution to cl_beta
c  cl_beta_vtail  vertical tail contribution to cl_beta
c  cl_beta_wingbody  wing-body contribution to cl_beta
c  cl_d         rolling effectiveness of partial-chord controls
c  cl_da        variation of rolling moment coefficient with aileron deflection
c  cl_dr        variation of rolling moment coefficient with rudder deflection
c  cn_beta      variation of yawing moment coefficient with sideslip angle
c  cn_beta_fuse  fuselage contribution to cn_beta
c  cn_beta_vtail  vertical tail contribution to cn_beta
c  cn_beta_wing  wing contribution to cn_beta
c  cn_da        variation of yawing moment coefficient with aileron deflection
c  cn_dr        variation of yawing moment coefficient with rudder deflection
c  cy_beta      variation of side force coefficient with sideslip angle
c  cy_beta_fuse  fuselage contribution to cy_beta
c  cy_beta_vtail  vertical tail contribution to cy_beta
c  cy_beta_wing  wing contribution to cy_beta
c  cy_da        variation of side force coefficient with aileron deflection
c  cy_dr        variation of side force coefficient with rudder deflection
c  da           aileron deflection, positive for right aileron up, left
c                aileron down (rad)
c  dr           rudder deflection, positive for right deflection (rad)
c  eqn_7_5      factor estimated by Equation 7.5
c  eqn_7_10     body-induced effect on wing height (from Equation 7.10)
c  eqn_7_11     d in Equation 7.10 (estimated from Equation 7.11)
c  eqn_7_12     another body-induced effect on wing height (from Equation 7.12)
c  eqn_11_2     rolling effectiveness of two full-chord ailerons (Eqn. 11.2)
c  f_cy_beta    correction factor for cy_beta
c  f_cl_beta    correction factor for cl_beta
c  f_cn_beta    correction factor for cn_beta
c  f_cl_da      correction factor for cl_da
c  f_cn_da      correction factor for cn_da
c  f_cy_dr      correction factor for cy_dr
c  f_cl_dr      correction factor for cl_dr
c  f_cn_dr      correction factor for cn_dr
c  fig_7_1      wing-body interference factor from Figure 7.1
c  fig_7_3      empirical factor from Figure 7.3
c  fig_7_5      ratio of the aspect ratio of the vertical panel in the presence
c                of the body to that of the isolated panel (from Figure 7.5)
c  fig_7_6      ratio of the vertical panel aspect ratio in the presence of
c                the horizontal tail and body to that of the panel in the
c                presence of the body alone (from Figure 7.6)
c  fig_7_7      factor accounting for relative size of horizontal and vertical
c                tails (from Figure 7.7)
c  fig_7_11     wing sweep contribution to cl_beta_wingbody (from Figure 7.11)
c  fig_7_12     compressibility correction to wing sweep (from Figure 7.12)
c  fig_7_13     fuselage correction factor (from Figure 7.13)
c  fig_7_14     aspect ratio contribution to cl_beta_wingbody (from Figure 7.14)
c  fig_7_15     dihedral effect on cl_beta (from Figure 7.15)
c  fig_7_16     compressibility correction to dihedral effect (from Figure 7.16)
c  fig_7_19     factor for body and body + wing effects (from Figure 7.19)
c  fig_7_20     Reynold's number factor for the fuselage (from Figure 7.20)
c  fig_10_2     flap chord factor (from Figure 10.2)
c  fig_10_3     span factor for plain flap (from Figure 10.3)
c  fig_10_5     theoretical lift effectiveness of plain TE flaps (Fig. 10.5)
c  fig_10_6     empirical correction for plain TE flaps (Fig. 10.6)
c  fig_10_7     empirical correction for lift effectiveness of plain flaps
c                at high flap deflections (from Figure 10.7)
c  fig_11_1     rolling moment effectiveness parameter (from Figure 11.1)
c  flap_eff_ratio  flap effectiveness ratio (from Figure 10.2)
c  i            index
c  k            empirical factor for estimating the variation of yawing
c                moment coefficient with aileron deflection
c  kappa        ratio of the actual lift-curve slope to 2*pi
c  phi          bank angle, positive to the right (rad)
c  q            dynamic pressure (lb/ft^2)
c  re_fuse      fuselage Reynolds number
c  sbs          body side area (ft^2)
c  sh           area of horizontal tail (ft^2)
c  sv           area of vertical tail (ft^2)
c  sweep_vtail_1_2  vertical tail half-chord sweep angle (deg)
c  x            temporary variable for curve fits
c
c  Created by:  Joel Grasmeyer
c  Last Modified:  10/28/97
c
c  I/O changes by W.H. Mason, 3/30/2000
c
c///////////////////////////////////////////////////////////////////////

      subroutine stab(outfile,title,write_flag,dihedral_wing,z_wing,
     & dia_fuse,sref,hspan_vtail,depth_fuse_vtail,c_vtail_root,
     & c_vtail_tip,mach_eo,sweep_vtail_1_4,sweep_wing_1_2,hspan_wing,
     & cl,z_vtail,l_vtail,length_fuse,new,nef,dia_nacelle,
     & sh,rho_eo,a_eo,mu_eo,dr_max,xbar,cbar,ar_vtail_eff,cn_avail)

      implicit none

      character*72 outfile, title
      integer i, write_flag, unit_out, unit_outfile, new, nef
      real pi, dihedral_wing, z_wing, dia_fuse, sref, hspan_vtail, ar,
     & depth_fuse_vtail, c_vtail_root, c_vtail_tip, mach_eo, sv, sh,
     & sweep_wing_1_2, hspan_wing, cy_beta, ar_vtail, k,
     & cy_beta_wing, cy_beta_fuse, cy_beta_vtail, ar_vtail_eff, alpha,
     & cl_alpha_vtail, beta_m, eqn_7_5, kappa, cl_alpha_vtail_eff, q,
     & cl_beta, cl_beta_htail, cl_beta_vtail, cl_beta_wingbody, sbs,
     & cl, fig_7_1, fig_7_5, fig_7_6, fig_7_7, fig_7_11, cn_da,
     & fig_7_12, fig_7_13, fig_7_14, fig_7_15, fig_7_16, eqn_7_10, da,
     & eqn_7_11, eqn_7_12, cl_alpha, z_vtail, l_vtail, cn_beta_fuse,
     & cn_beta_vtail, cn_beta, cn_beta_wing, fig_7_19, fig_7_20, phi,
     & length_fuse, re_fuse, cl_da, cy_da, fig_11_1, eqn_11_2, cl_d,
     & alpha_d, fig_10_5, fig_10_6, cl_alpha_2d, cl_dr, dr,
     & cn_dr, cy_dr, fig_10_2, fig_10_7, alpha_d_cl, flap_eff_ratio,
     & fig_10_3, cf_c, beta, rho_eo, a_eo, mu_eo, cn_avail, fig_7_3,
     & dia_nacelle, dr_max, f_cy_beta, f_cl_beta,
     & f_cn_beta, f_cl_da, f_cn_da, f_cy_dr, f_cl_dr, f_cn_dr, x,
     & sweep_vtail_1_4, sweep_vtail_1_2,
     & cnbw1, cnbw2, cnbw3, cnbw4, cnbw5, cnbw6, cn_beta_wingterm, 
     & cbar, xbar

      pi = acos(-1.)

c Convert sweep angles from degrees to radians
cwhm THIS IS A VERY DANGEROUS PRACTICE!! DON'T USE AS AN EXAMPLE
cwhm of acceptable coding style
      sweep_wing_1_2 = sweep_wing_1_2*pi/180.
      sweep_vtail_1_4 = sweep_vtail_1_4*pi/180.

c Write input data to screen for confirmation
      if (write_flag .eq. 1) then
        unit_out = 6
        write(unit_out,90) title
        write(unit_out,*)
        write(unit_out,"(a25)") '   Input Data'
        write(unit_out,*)
        write(unit_out,101) write_flag, '= write_flag'
        write(unit_out,100) dihedral_wing, '= dihedral_wing'
        write(unit_out,100) z_wing, '= z_wing'
        write(unit_out,100) dia_fuse, '= dia_fuse'
        write(unit_out,100) sref, '= sref'
        write(unit_out,100) hspan_vtail, '= hspan_vtail'
        write(unit_out,100) depth_fuse_vtail, '= depth_fuse_vtail'
        write(unit_out,100) c_vtail_root, '= c_vtail_root'
        write(unit_out,100) c_vtail_tip, '= c_vtail_tip'
        write(unit_out,100) mach_eo, '= mach_eo'
        write(unit_out,100) sweep_vtail_1_4*180./pi, '= sweep_vtail_1_4'
        write(unit_out,100) sweep_wing_1_2*180./pi, '= sweep_wing_1_2'
        write(unit_out,100) hspan_wing, '= hspan_wing'
        write(unit_out,100) cl, '= cl'
        write(unit_out,100) z_vtail, '= z_vtail'
        write(unit_out,100) l_vtail, '= l_vtail'
        write(unit_out,100) length_fuse, '= length_fuse'
        write(unit_out,101) new, '= new'
        write(unit_out,101) nef, '= nef'
        write(unit_out,100) dia_nacelle, '= dia_nacelle'
        write(unit_out,100) sh, '= sh'
        write(unit_out,100) rho_eo, '= rho_eo'
        write(unit_out,100) a_eo, '= a_eo'
        write(unit_out,103) mu_eo, '= mu_eo'
        write(unit_out,100) dr_max, '= dr_max'
	  write(unit_out,100) xbar, '= xbar'
	  write(unit_out,100) cbar, '= cbar'
      end if

c Echo input to output file
      unit_outfile = 20
	write(unit_outfile,90) title
      write(unit_outfile,*)
      write(unit_outfile,"(a25)") '   Input Data'
      write(unit_outfile,*)
      write(unit_outfile,101) write_flag, '= write_flag'
      write(unit_outfile,100) dihedral_wing, '= dihedral_wing'
      write(unit_outfile,100) z_wing, '= z_wing'
      write(unit_outfile,100) dia_fuse, '= dia_fuse'
      write(unit_outfile,100) sref, '= sref'
      write(unit_outfile,100) hspan_vtail, '= hspan_vtail'
      write(unit_outfile,100) depth_fuse_vtail, '= depth_fuse_vtail'
      write(unit_outfile,100) c_vtail_root, '= c_vtail_root'
      write(unit_outfile,100) c_vtail_tip, '= c_vtail_tip'
      write(unit_outfile,100) mach_eo, '= mach_eo'
      write(unit_outfile,100) sweep_vtail_1_4*180./pi,'=sweep_vtail_1_4'
      write(unit_outfile,100) sweep_wing_1_2*180./pi, '= sweep_wing_1_2'
      write(unit_outfile,100) hspan_wing, '= hspan_wing'
      write(unit_outfile,100) cl, '= cl'
      write(unit_outfile,100) z_vtail, '= z_vtail'
      write(unit_outfile,100) l_vtail, '= l_vtail'
      write(unit_outfile,100) length_fuse, '= length_fuse'
      write(unit_outfile,101) new, '= new'
      write(unit_outfile,101) nef, '= nef'
      write(unit_outfile,100) dia_nacelle, '= dia_nacelle'
      write(unit_outfile,100) sh, '= sh'
      write(unit_outfile,100) rho_eo, '= rho_eo'
      write(unit_outfile,100) a_eo, '= a_eo'
      write(unit_outfile,103) mu_eo, '= mu_eo'
      write(unit_outfile,100) dr_max, '= dr_max'
	write(unit_outfile,100) xbar, '= xbar'
	write(unit_outfile,100) cbar, '= cbar'

c Calculate stability and control derivatives via Roskam's methods

c Sideslip angle derivatives
      cy_beta_wing = -0.0001*abs(dihedral_wing)*180./pi

c Estimate fig_7_1 from Figure 7.1 (curve fit)
      if (z_wing/(dia_fuse/2.) .le. 0.) then
        fig_7_1 = 0.85*(-z_wing/(dia_fuse/2.)) + 1.
      elseif (z_wing/(dia_fuse/2.) .gt. 0.) then
        fig_7_1 = 0.5*z_wing/(dia_fuse/2.) + 1.
      end if

c Estimate the side force coefficient due to the fuselage and nacelles
      cy_beta_fuse = -2.*fig_7_1*( pi*(dia_fuse/2.)**2 +
     & (new + nef)*pi*(dia_nacelle/2.)**2 )/sref

c Estimate fig_7_3 from Figure 7.3 (curve fit)
      x = hspan_vtail/depth_fuse_vtail
      if (x .le. 2.) then
        fig_7_3 = 0.75
      elseif (x .gt. 2. .and. x .lt. 3.5) then
        fig_7_3 = x/6. + 5./12.
      elseif (x .ge. 3.5) then
        fig_7_3 = 1.
      end if

c Estimate fig_7_5 from Figure 7.5 (curve fit for taper ratio <= 0.6)
      x = hspan_vtail/depth_fuse_vtail
      fig_7_5 = 0.002*x**5 - 0.0464*x**4 + 0.404*x**3 - 1.6217*x**2 +
     & 2.7519*x + 0.0408

c Factor from Figure 7.6 is for zh/bv = 0.
      fig_7_6 = 1.1

c Estimate fig_7_7 from Figure 7.7 (curve fit)
      sv = hspan_vtail*(c_vtail_root + c_vtail_tip)/2.
      x = sh/sv
      fig_7_7 = -0.0328*x**4 + 0.2885*x**3 - 0.9888*x**2 + 1.6554*x -
     & 0.0067

c Estimate the effective aspect ratio for the vertical tail
      ar_vtail = hspan_vtail**2/sv
      ar_vtail_eff = fig_7_5*ar_vtail*(1. + fig_7_7*(fig_7_6 - 1.))

c Assume the section lift-curve slope is 2.*pi
      cl_alpha_vtail = 2.*pi

c Estimate the effective lift-curve slope for the vertical tail
      kappa = cl_alpha_vtail/(2.*pi)
      beta_m = sqrt( 1. - mach_eo**2 )
      sweep_vtail_1_2 = atan( (c_vtail_root/4. + hspan_vtail*
     & tan(sweep_vtail_1_4) + c_vtail_tip/4. - c_vtail_root/2.)/
     & hspan_vtail )
      cl_alpha_vtail_eff = 2*pi*ar_vtail_eff/( 2. +
     & sqrt( ar_vtail_eff**2*beta_m**2/kappa**2*( 1. +
     & tan(sweep_vtail_1_2)**2/beta_m**2 ) + 4. ) )

c Estimate the third term in eqn. 7.4 from eqn. 7.5
      eqn_7_5 = 0.724 + 3.06*sv/sref/(1. + cos(sweep_vtail_1_4)) +
     & 0.4*z_wing/dia_fuse + 0.009*ar_vtail_eff
      cy_beta_vtail = -fig_7_3*cl_alpha_vtail_eff*eqn_7_5*sv/sref

c Calculate total variation of side force coefficient with sideslip angle
      cy_beta = cy_beta_wing + cy_beta_fuse + cy_beta_vtail

c Factor from Figure 7.11 is approximated by a curve fit for lambda = 0.5
      fig_7_11 = -0.004/45*sweep_wing_1_2*180./pi

c Factor from Figure 7.12 is approximated for 747 and 777 configurations
c at low Mach numbers
      fig_7_12 = 1.0

c Factor from Figure 7.13 is approximated for 747 and 777 configurations
      fig_7_13 = 0.85

c Factor from Figure 7.14 is approximated for lambda = 0.5 and high AR
      fig_7_14 = 0.000

c Factor from Figure 7.15 is approximated by a linear curve fit for
c  lambda = 0.5, low sweep, and high AR
      ar = (2.*hspan_wing)**2/sref
      fig_7_15 = -0.00012 - 0.00013/10*ar

c Factor from Figure 7.16 is approximated for 747 and 777 configurations
c at low Mach numbers
      fig_7_16 = 1.0

c Estimate body-induced effect on wing height from eqns. 7.10, 7.11, and 7.12
      eqn_7_11 = sqrt(pi*(dia_fuse/2.)**2/0.7854)
      eqn_7_10 = -0.0005*sqrt(ar)*(eqn_7_11/(2.*hspan_wing))**2
      eqn_7_12 = -1.2*sqrt(ar)/(180./pi)*z_wing/(2.*hspan_wing)*
     & 2.*eqn_7_11/(2.*hspan_wing)

c Wing-body contribution to cl_beta (wing twist effect is neglected)
      cl_beta_wingbody = ( cl*(fig_7_11*fig_7_12*fig_7_13 +
     & fig_7_14) + dihedral_wing*(fig_7_15*fig_7_16 + eqn_7_10) +
     & eqn_7_12 )*180./pi

c Since the horizontal tail has a small lift coefficient, small dihedral,
c  and small area relative to the wing, it is negligible.
      cl_beta_htail = 0.

c Estimate the angle of attack from the CL by assuming cl_alpha = 5.7
c based on the value given for the 747 in Nelson.  The angle of attack
c of the fuselage reference line is given as 5.7 deg for the M=0.25
c flight condition in NASA CR-2144.  Therefore, the effective wing
c incidence angle (with flaps at 20 deg) is 5.457 deg.
      cl_alpha = 5.7
      alpha = cl/cl_alpha - 5.457*pi/180.

c Estimate the vertical tail contribution to cl_beta
      cl_beta_vtail = cy_beta_vtail*( z_vtail*cos(alpha) - l_vtail*
     & sin(alpha) )/(2.*hspan_wing)

c Calculate total variation of rolling moment coefficient with sideslip angle
      cl_beta = cl_beta_wingbody + cl_beta_htail + cl_beta_vtail

c Wing contribution to cn_beta is negligible for small angles of attack.
c      cn_beta_wing = 0.
c Calculate contribution of wing to cn_beta using DATCOM method.
c   Code added by L.T. Leifsson, code written by W.H. Mason, March 2004. 
	cnbw1 = 1.0/(4.0*pi*ar)
	cnbw2 = tan(sweep_wing_1_2)/pi/ar/(ar + 4.0*cos(sweep_wing_1_2))
	cnbw3 = cos(sweep_wing_1_2)
	cnbw4 = ar/2.0
	cnbw5 = ar**2.0/8.0/cos(sweep_wing_1_2)
	cnbw6 = 6.0*xbar/cbar*sin(sweep_wing_1_2)/ar
	cn_beta_wingterm = cnbw1 - cnbw2*(cnbw3 - cnbw4 - cnbw5 + cnbw6)
	cn_beta_wing = cl**2.0*cn_beta_wingterm

c Estimate empirical factor for body and body + wing effects from Figure 7.19
c  Constant value assumed for 747 and 777-like configurations
      fig_7_19 = 0.0011

c Calculate fuselage Reynolds number at the engine-out flight condition
      re_fuse = rho_eo*mach_eo*a_eo*length_fuse/mu_eo

c Estimate fuselage Reynolds number effect on wing-body from Figure 7.20
      fig_7_20 = 1. + 1.2/log(350.)*log(re_fuse/1000000.)

c Estimate fuselage contribution to cn_beta
      sbs = 0.83*dia_fuse*length_fuse
      cn_beta_fuse = -180./pi*fig_7_19*fig_7_20*sbs/sref*
     & length_fuse/(2.*hspan_wing)

c Estimate vertical tail contribution to cn_beta
      cn_beta_vtail = -cy_beta_vtail*( l_vtail*cos(alpha) +
     & z_vtail*sin(alpha) )/(2.*hspan_wing)

c Calculate total variation of rolling moment coefficient with sideslip angle
      cn_beta = cn_beta_wing + cn_beta_fuse + cn_beta_vtail

c Assume variation of sideforce coefficient with aileron deflection is zero
      cy_da = 0.

c Estimate the rolling moment effectiveness parameter from Figure 11.1
c  for lambda = 0.5, and for 747 and 777-like ailerons at mach 0.2
      fig_11_1 = 0.18

c Assume the 2D lift-curve slope is 2*pi/beta_m
      cl_alpha_2d = 2*pi/beta_m
      kappa = cl_alpha_2d/(2.*pi/beta_m)

c Estimate the rolling effectiveness of two full-chord controls by Eqn. 11.2
      eqn_11_2 = kappa/beta_m*fig_11_1

c Estimate aileron effectiveness by assuming cf/c = 0.20 and t/c = 0.08
      fig_10_5 = 3.5
      fig_10_6 = 1.0
      cl_d = fig_10_6*fig_10_5
      alpha_d = cl_d/cl_alpha_2d

c Determine the rolling effectiveness of the partial-chord controls by
c Eqn. 11.3.  Note that this is the change in cl with respect to a change
c in the sum of the left and right aileron deflections (d).
      cl_d = alpha_d*eqn_11_2

c Estimate variation of rolling moment coefficient with aileron deflection
c by neglecting differential control effects.  Since the aileron deflection
c (da) is defined as half of the sum of the left and right deflections, cl_d
c from the equation above must be divided by 2.
      cl_da = cl_d/2.

c The method in Roskam for estimating cn_da does not account for the
c effect of differential ailerons and the use of spoilers for roll control
c on the yaw moment.  Therefore, the factor k is estimated
c based on the ratio of cn_da to cl_da from the 747 flight test data
c presented in Nelson.  Note that the effect of cl is absorbed into
c the factor k.
      k = 0.0064/0.0461

c Estimate variation of yawing moment coefficient with aileron deflection
      cn_da = k*cl_da

c Estimate the flap chord factor from Figure 10.2 for cf/c = 0.33
c  The flap effectiveness ratio is estimated with a piecewise curve fit
      cf_c = 0.33
      alpha_d_cl = -sqrt( 1. - (1. - cf_c)**2 )
      if (alpha_d_cl .ge. -0.5) then
        flap_eff_ratio = 1.42 + 1.8*alpha_d_cl
      elseif (alpha_d_cl .ge. -0.6) then
        flap_eff_ratio = 1.32 + 1.6*alpha_d_cl
      elseif (alpha_d_cl .ge. -0.7) then
        flap_eff_ratio = 1.08 + 1.2*alpha_d_cl
      else
        flap_eff_ratio = 0.94 + alpha_d_cl
      end if
      flap_eff_ratio = 1. + flap_eff_ratio/( ar_vtail_eff -
     & 0.5*(-alpha_d_cl - 2.1) )
      fig_10_2 = flap_eff_ratio*alpha_d_cl

c Estimate empirical correction for lift effectiveness of plan flaps at
c from Figure 10.7 for cf/c = 0.33.
      x = dr_max
      if (x .lt. 15.) then
        fig_10_7 = 1.
      else
        fig_10_7 = 4e-7*x**4 - 7e-5*x**3 + 0.0047*x**2 - 0.1453*x +
     & 2.3167
      end if

c Estimate span factor for plain flap from Figure 10.3 for delta eta = 0.85
      fig_10_3 = 0.95

c Estimate variation of sideforce coefficient with rudder deflection
      cy_dr = cl_alpha_vtail_eff*fig_10_2*fig_10_7*fig_10_3*sv/sref

c Estimate variation of rolling moment coefficient with rudder deflection
      cl_dr = cy_dr*( z_vtail*cos(alpha) - l_vtail*sin(alpha) )/
     & (2.*hspan_wing)

c Estimate variation of yawing moment coefficient with rudder deflection
      cn_dr = -cy_dr*( l_vtail*cos(alpha) + z_vtail*sin(alpha) )/
     & (2.*hspan_wing)

c Multiply empirical estimates by their respective correction factors
c The correction factors are the ratio of the actual 747 derivatives to
c the 747 derivatives predicted by the method above at the M=0.25 flight
c condition defined in NASA CR-2144 and Nelson.  The rudder deflection
c was 15 deg for this calibration.
      cy_beta = 1.4068*cy_beta
      cl_beta = 0.7253*cl_beta
      cn_beta = 1.4232*cn_beta	! New correction factor, Cnbetawing now 
      cl_da   = 0.9202*cl_da		! estimated. Assuming xbar = 0.0 ft and
      cn_da   = 0.9143*cn_da		! cbar = 27.31 ft, for 747,Leifsson, March '04.
      cy_dr   = 0.6132*cy_dr
      cl_dr   = 0.3004*cl_dr
      cn_dr   = 0.7320*cn_dr

c Set the rudder deflection to 20 deg, and the bank angle to 5 deg
      dr = dr_max*pi/180.
      phi = 5.*pi/180.

c Solve for the sideslip angle and aileron deflection
      beta = -( cy_dr*dr + cl*sin(phi) )/cy_beta
      da = -( cl_dr*dr + cl_beta*beta)/cl_da

c Calculate the maximum available yawing moment coefficient
      cn_avail = cn_da*da + cn_dr*dr + cn_beta*beta

c Write output data
      if (write_flag .eq. 1) then
        write(unit_out,*)
        write(unit_out,"(a26)") '   Output Results'
        write(unit_out,96)
        write(unit_out,104) cy_beta_wing, '= cy_beta_wing'
        write(unit_out,104) cy_beta_fuse, '= cy_beta_fuse'
        write(unit_out,104) cy_beta_vtail, '= cy_beta_vtail'
        write(unit_out,*)
        write(unit_out,105) cy_beta, '= cy_beta'
        write(unit_out,*)
        write(unit_out,104) cl_beta_wingbody, '= cl_beta_wingbody'
        write(unit_out,104) cl_beta_htail, '= cl_beta_htail'
        write(unit_out,104) cl_beta_vtail, '= cl_beta_vtail'
        write(unit_out,*)
        write(unit_out,105) cl_beta, '= cl_beta'
        write(unit_out,*)
        write(unit_out,104) cn_beta_wing, '= cn_beta_wing'
        write(unit_out,104) cn_beta_fuse, '= cn_beta_fuse'
        write(unit_out,104) cn_beta_vtail, '= cn_beta_vtail'
        write(unit_out,*)
        write(unit_out,105) cn_beta, '= cn_beta'
        write(unit_out,97)
        write(unit_out,100) cy_da, '= cy_da'
        write(unit_out,100) cl_da, '= cl_da'
        write(unit_out,100) cn_da, '= cn_da'
        write(unit_out,*)
        write(unit_out,100) cy_dr, '= cy_dr'
        write(unit_out,100) cl_dr, '= cl_dr'
        write(unit_out,100) cn_dr, '= cn_dr'
        write(unit_out,98)
        write(unit_out,100) beta*180./pi, '= beta (deg)'
        write(unit_out,100) phi*180./pi, '= phi (deg)'
        write(unit_out,100) da*180./pi, '= da (deg)'
        write(unit_out,100) dr*180./pi, '= dr (deg)'
        write(unit_out,100) ar_vtail_eff, '= ar_vtail_eff'
        write(unit_out,100) cn_avail, '= cn_avail'
        write(unit_out,99)
      endif

c Write results to output file
      write(unit_outfile,*)
      write(unit_outfile,"(a26)") '   Output Results'
      write(unit_outfile,96)
      write(unit_outfile,104) cy_beta_wing, '= cy_beta_wing'
      write(unit_outfile,104) cy_beta_fuse, '= cy_beta_fuse'
      write(unit_outfile,104) cy_beta_vtail, '= cy_beta_vtail'
      write(unit_outfile,*)
      write(unit_outfile,105) cy_beta, '= cy_beta'
      write(unit_outfile,*)
      write(unit_outfile,104) cl_beta_wingbody, '= cl_beta_wingbody'
      write(unit_outfile,104) cl_beta_htail, '= cl_beta_htail'
      write(unit_outfile,104) cl_beta_vtail, '= cl_beta_vtail'
      write(unit_outfile,*)
      write(unit_outfile,105) cl_beta, '= cl_beta'
      write(unit_outfile,*)
      write(unit_outfile,104) cn_beta_wing, '= cn_beta_wing'
      write(unit_outfile,104) cn_beta_fuse, '= cn_beta_fuse'
      write(unit_outfile,104) cn_beta_vtail, '= cn_beta_vtail'
      write(unit_outfile,*)
      write(unit_outfile,105) cn_beta, '= cn_beta'
      write(unit_outfile,97)
      write(unit_outfile,100) cy_da, '= cy_da'
      write(unit_outfile,100) cl_da, '= cl_da'
      write(unit_outfile,100) cn_da, '= cn_da'
      write(unit_outfile,*)
      write(unit_outfile,100) cy_dr, '= cy_dr'
      write(unit_outfile,100) cl_dr, '= cl_dr'
      write(unit_outfile,100) cn_dr, '= cn_dr'
      write(unit_outfile,98)
      write(unit_outfile,100) beta*180./pi, '= beta (deg)'
      write(unit_outfile,100) phi*180./pi, '= phi (deg)'
      write(unit_outfile,100) da*180./pi, '= da (deg)'
      write(unit_outfile,100) dr*180./pi, '= dr (deg)'
      write(unit_outfile,100) ar_vtail_eff, '= ar_vtail_eff'
      write(unit_outfile,100) cn_avail, '= cn_avail'
      write(unit_outfile,99)

c Close output file
	close(unit_outfile)
c Write statements and formats
   90 format(/4x,'Data set title: ',a72)
   96 format(/3x,'stability derivatives'/)
   97 format(/3x,'control derivatives'/)
   98 format(/3x,'engine out information - 5 deg bank and max rudder'/)
   99 format(/3x,'Calculations completed')
  100 format(3x,f11.4, 1x, a)
  101 format(10x, i4, 1x, a)
  102 format(3x,f11.0, 1x, a)
  103 format(3x,g11.4, 1x, a)
  104 format(22x,f11.4, 1x, a)
  105 format(3x,f11.4, 1x, a, ' (adjusted)')

c Convert sweep angles from radians back to degrees
cwhm THIS IS A VERY DANGEROUS PRACTICE!! DON'T USE AS AN EXAMPLE
cwhm of acceptable coding style
      sweep_wing_1_2 = sweep_wing_1_2*180./pi
      sweep_vtail_1_4 = sweep_vtail_1_4*180./pi

      return
      end

