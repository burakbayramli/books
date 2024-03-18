c///////////////////////////////////////////////////////////////////////
c
c  subroutine idrag
c
c    This subroutine calculates the distribution of the normal force
c  coefficient, induced drag, and span efficiency factor for nonplanar
c  wings composed of multiple panels.  It is based on the paper
c  entitled "Numerical Method to Calculate the Induced Drag or Optimum
c  Loading for Arbitrary Non-planar Aircraft" by James Blackwell, Jr.,
c  from NASA SP-405, Vortex-Lattice Utilization
c
c  Inputs
c
c  outfile      output filename
c  title        title of aircraft configuration
c  input_mode   input mode (0 = design unknown loads, 1 = analyze given loads)
c  write_flag   write flag (0 = no output file, 1 = output file written)
c  sym_flag     symmetry flag (0 = asymmetric, 1 = symmetric)
c  cl_design    design lift coefficient
c  cm_flag      flag for Cm constraint (0 = no constraint, 1 = constraint)
c  cm_design    design pitching moment coefficient (about the cg)
c  xcg          x location of the center of gravity
c  cp           percent chord location of center of pressure for all sections
c  sref         reference area (projected onto xy plane)
c  cavg         average chord of reference surface(s)
c  npanels      number of panels in geometry
c  xc, yc, zc   coordinates of corner points (aircraft reference frame)
c  nvortices    number of vortices per panel
c  spacing_flag vortex spacing flag (0 = equal, 1 = outboard-compressed,
c                2 = inboard-compressed, 3 = end-compressed)
c  load_flag    load flag (0 = cn input, 1 = load (cn*c/cavg) input)
c  loads        vector of loads for analysis mode
c
c  Outputs
c
c  cd_induced   induced drag coefficient
c
c  Internal Variables
c
c  abar         augmented influence coefficient matrix (Lamar)
c  ar           aspect ratio of reference surface
c  b            vector of constraints; overwritten with loads vector
c  bref         reference span (projected onto xy plane)
c  c            local chord of lifting element
c  cl_actual    actual lift coefficient (obtained via integration)
c  cm_actual    actual pitching moment coefficient (about the cg)
c  cn           vector of normal force coefficients
c  d            dummy output from LU decomposition subroutine
c  e            span efficiency factor
c  i            index
c  info         SGEFS information flag
c  ipvt         SGEFS output vector of row interchanges
c  j            index
c  lda          leading dimension of abar matrix
c  ldb          leading dimension of b matrix
c  n            actual size of abar matrix
c  nb           number of columns in b matrix
c  nconstraints number of constraints (parameter)
c  npanels_max  maximum number of panels in geometry
c  nvortices_max  maximum number of vortices per panel
c  nvortices_tot  total number of vortices
c  s            vortex semi-width (non-dimensional)
c  semi_width   temporary parameter for calculating vortex semi-width
c  sp           vortex semi-width (dimensional)
c  spacing      vortex spacing
c  theta        vortex dihedral angle
c  unit_write   unit number for write statements
c  x, y, z      coordinates of vortices (aircraft reference frame)
c  xle, xte     coordinates of LE and TE corresponding to each vortex
c
c  Created by:  Joel Grasmeyer
c  Last Modified:  02/10/97
c  Version:  1.1
c  Please send any bug reports to grasmeye@aoe.vt.edu
c
c///////////////////////////////////////////////////////////////////////

      subroutine idrag(outfile,title,input_mode,write_flag,sym_flag,
     & cl_design,cm_flag,cm_design,xcg,cp,sref,cavg,npanels,xc,yc,zc,
     & nvortices,spacing_flag,load_flag,loads,cd_induced)

      integer npanels_max, nvortices_max, nconstraints, unit_write
      parameter(npanels_max=5, nvortices_max=200, nconstraints=2,
     & unit_write=11)

      character*72 outfile, title
      integer nvortices_tot, n, i, j, cm_flag, input_mode,
     & write_flag, sym_flag, npanels, nvortices(npanels_max),
     & spacing_flag(npanels_max), load_flag, lda, ldb, nb, info,
     & ipvt(npanels_max*nvortices_max+nconstraints)
      real cl_design, sref, bref, cavg, x(npanels_max*nvortices_max),
     & y(npanels_max*nvortices_max), z(npanels_max*nvortices_max),
     & c(npanels_max*nvortices_max), s(npanels_max*nvortices_max),
     & sp(npanels_max*nvortices_max), theta(npanels_max*nvortices_max),
     & a(npanels_max*nvortices_max,npanels_max*nvortices_max), pi,
     & b(npanels_max*nvortices_max+nconstraints), cp, cm_actual,
     & abar(npanels_max*nvortices_max+nconstraints,
     & npanels_max*nvortices_max+nconstraints), cl_actual, e,
     & xle(npanels_max*nvortices_max), cd_induced, ar, xcg,
     & cn(npanels_max*nvortices_max), xc(npanels_max,4),
     & loads(npanels_max*nvortices_max), yc(npanels_max,4),
     & spacing, zc(npanels_max,4), semi_width, cm_design,
     & xte(npanels_max*nvortices_max)

      pi = acos(-1.)

c Write input data to output file for confirmation
      if (write_flag .eq. 1) then
        open(unit_write,file=outfile)
        write(unit_write,101)
        write(unit_write,102) title
        write(unit_write,103) input_mode, '= input mode'
        write(unit_write,103) write_flag, '= write flag'
        write(unit_write,103) sym_flag, '= symmetry flag'
        write(unit_write,104) cl_design, '= design lift coefficient'
        write(unit_write,103) cm_flag, '= moment coefficient flag'
        write(unit_write,104) cm_design, '= design moment coefficient'
        write(unit_write,104) xcg, '= x cg position'
        write(unit_write,104) cp,
     & '= center of pressure for airfoil sections'
        write(unit_write,104) sref, '= reference area'
        write(unit_write,104) cavg, '= reference chord'
        write(unit_write,103) npanels, '= number of panels'
        do 100 i=1,npanels
          write(unit_write,*)
          write(unit_write,105)
     & '    x        y        z    for panel ', i
          do 200 j=1,4
            write(unit_write,106) xc(i,j), yc(i,j), zc(i,j)
  200     continue
          write(unit_write,103) nvortices(i), '= number of vortices'
          write(unit_write,103) spacing_flag(i), '= vortex spacing flag'
  100   continue
  101   format('idrag output file')
  102   format(a72)
  103   format(2x, i3, 4x, a)
  104   format(f8.2, 1x, a)
  105   format(a, i1)
  106   format(3(f8.2, 1x))
      end if

c Calculate reference span
      bref = sref/cavg

c Begin vortex loop
      nvortices_tot = 0
      do 400 i=1,npanels
        do 500 j=1,nvortices(i)
          nvortices_tot = nvortices_tot + 1

c Calculate dihedral angles of lifting elements
          if (yc(i,2) .eq. yc(i,1)) then
            theta(nvortices_tot) = pi/2.
          else
            theta(nvortices_tot) = atan((zc(i,2) - zc(i,1))/
     &       (yc(i,2) - yc(i,1)))
          end if

c Calculate spacing (0 = equal, 1 = outboard-compressed, 2 = inboard-
c   compressed, 3 = end-compressed) and panel semi-widths
          if (spacing_flag(i) .eq. 0) then
            spacing = (j - 0.5)/nvortices(i)
            semi_width = 0.5/nvortices(i)
          elseif (spacing_flag(i) .eq. 1) then
            spacing = sin(pi/2.*(j - 0.5)/nvortices(i))
            semi_width = 0.5*(sin(pi/2.*j/nvortices(i)) -
     &       sin(pi/2.*(j - 1.)/nvortices(i)))
          elseif (spacing_flag(i) .eq. 2) then
            spacing = 1 - cos(pi/2.*(j - 0.5)/nvortices(i))
            semi_width = 0.5*(-cos(pi/2.*j/nvortices(i)) +
     &       cos(pi/2.*(j - 1.)/nvortices(i)))
          elseif (spacing_flag(i) .eq. 3) then
            spacing = (1 - cos(pi*(j - 0.5)/nvortices(i)))/2.
            semi_width = 0.25*(-cos(pi*j/nvortices(i)) +
     &       cos(pi*(j - 1.)/nvortices(i)))
          end if

          sp(nvortices_tot) = semi_width*sqrt((yc(i,2) -
     &     yc(i,1))**2 + (zc(i,2) - zc(i,1))**2)
          s(nvortices_tot) = 2.*sp(nvortices_tot)/bref

c Calculate coordinates of vortices and local chords
          y(nvortices_tot) = yc(i,1) + spacing*(yc(i,2) - yc(i,1))
          z(nvortices_tot) = zc(i,1) + spacing*(zc(i,2) - zc(i,1))
          xle(nvortices_tot) = xc(i,1) + spacing*(xc(i,2) - xc(i,1))
          xte(nvortices_tot) = xc(i,4) + spacing*(xc(i,3) - xc(i,4))
          c(nvortices_tot) = (xte(nvortices_tot) -
     &     xle(nvortices_tot))
          x(nvortices_tot) = xle(nvortices_tot) + 0.25*c(nvortices_tot)
  500   continue
  400 continue

c Form matrix of influence coefficients with constraints
      call matrix(sym_flag,cl_design,cm_flag,cm_design,xcg,cp,
     & nvortices_tot,y,z,theta,s,sp,c,cavg,xle,a,abar,b)

c Design mode:  calculate loading for minimum induced drag
      if (input_mode .eq. 0) then
        if (cm_flag .eq. 0) then
          n = nvortices_tot+1
        else
          n = nvortices_tot+2
        end if
        lda = npanels_max*nvortices_max+nconstraints
        ldb = lda
        nb = 1

c Calculate the loads (note that the b vector is an input and output)
        call sgefs(abar,lda,n,b,ldb,nb,ipvt,info)

c Analysis mode:  calculate performance for given loading
      else

c If Cn values were input, convert to loads (Cn*c/cavg)
        if (load_flag .eq. 0) then
          do 600 i=1,nvortices_tot
            loads(i) = loads(i)*c(i)/cavg
  600     continue
        end if

c Set b vector to given loads
        do 700 i=1,nvortices_tot
          b(i) = loads(i)
  700   continue
      end if

c Calculate actual lift coefficient, induced drag, and span efficiency
      cl_actual = 0.
      cm_actual = 0.
      cd_induced = 0.
      do 800 i=1,nvortices_tot
        if (sym_flag .eq. 1) then
          cl_actual = cl_actual + 2.*b(i)*s(i)*cos(theta(i))
          cm_actual = cm_actual + 2.*b(i)*s(i)*cos(theta(i))*
     &     (xcg - (xle(i) + cp*c(i)))/cavg
        else
          cl_actual = cl_actual + b(i)*s(i)*cos(theta(i))
          cm_actual = cm_actual + b(i)*s(i)*cos(theta(i))*
     &     (xcg - (xle(i) + cp*c(i)))/cavg
        end if
        cn(i) = b(i)*cavg/c(i)
        do 900 j=1,nvortices_tot
          if (sym_flag .eq. 1) then
            cd_induced = cd_induced + b(i)*b(j)*s(i)*a(i,j)
          else
            cd_induced = cd_induced + 0.5*b(i)*b(j)*s(i)*a(i,j)
          end if
  900   continue
  800   continue
      ar = bref**2/sref
      e = cl_actual**2/(pi*ar*cd_induced)

c Write vortex positions, load distribution and performance to output file
      if (write_flag .eq. 1) then
        write(unit_write,*)
        write(unit_write,110)
     & '    i   x        y        z        load     cn'
        do 1000 i=1,nvortices_tot
          write(unit_write,111) i, x(i), y(i), z(i), b(i), cn(i)
 1000   continue
        write(unit_write,*)
        write(unit_write,112) cl_actual, '= actual lift coefficient'
        write(unit_write,112) cm_actual, '= actual moment coefficient'
        write(unit_write,112) cd_induced, '= induced drag coefficient'
        write(unit_write,112) e, '= span efficiency factor'
  110   format(a)
  111   format(x, i4, 5(1x, f8.4))
  112   format(4x, f7.5, 1x, a)
        close(11)
      end if

      return
      end


c///////////////////////////////////////////////////////////////////////
c
c  subroutine matrix
c
c    This subroutine forms the matrix of influence coefficients from
c  the geometry information provided by the idrag subroutine.  The
c  matrix equation (Ax = B) is then solved via the subroutines ludcmp
c  and lubksb.
c
c  Inputs
c
c  sym_flag     symmetry flag (0 = asymmetric, 1 = symmetric)
c  cl_design    design lift coefficient
c  cm_flag      flag for Cm constraint (0 = no constraint, 1 = constraint)
c  cm_design    design pitching moment coefficent (about the cg)
c  xcg          x location of the center of gravity
c  cp           percent chord location of center of pressure for all sections
c  nvortices_tot  total number of vortices
c  y, z         coordinates of vortices (aircraft reference frame)
c  theta        vortex dihedral angle
c  s            vortex semi-width (non-dimensional)
c  sp           vortex semi-width (dimensional)
c  c            local chord of lifting element
c  cavg         average chord of reference surface(s)
c  xle          coordinates of LE corresponding to each vortex
c
c  Outputs
c
c  a            influence coefficient matrix (Blackwell)
c  abar         augmented influence coefficient matrix (Lamar)
c  b            vector of constraints
c
c  Internal Variables
c
c  a1           matrix of influence coefficients for same-side vortices
c  a2           matrix of influence coefficients for image vortices
c  i            index
c  j            index
c  nconstraints number of constraints (parameter)
c  npanels_max  maximum number of panels in geometry
c  nvortices_max  maximum number of vortices per panel
c  r1, r2       vortex influence radii squared
c  yp, zp       coordinates of vortices (vortex reference frame)
c
c  Created by:  Joel Grasmeyer
c  Last Modified:  02/10/97
c
c///////////////////////////////////////////////////////////////////////

      subroutine matrix(sym_flag,cl_design,cm_flag,cm_design,xcg,cp,
     & nvortices_tot,y,z,theta,s,sp,c,cavg,xle,a,abar,b)

      integer npanels_max, nvortices_max, nconstraints
      parameter(npanels_max=5,nvortices_max=200,nconstraints=2)

      integer i, j, nvortices_tot, cm_flag, sym_flag
      real pi, yp, zp, y(npanels_max*nvortices_max), r1, r2, a1,
     & z(npanels_max*nvortices_max), theta(npanels_max*nvortices_max),
     & s(npanels_max*nvortices_max), sp(npanels_max*nvortices_max),
     & a2, a(npanels_max*nvortices_max,npanels_max*nvortices_max),
     & abar(npanels_max*nvortices_max+nconstraints,
     & npanels_max*nvortices_max+nconstraints), cp, cm_design, xcg,
     & b(npanels_max*nvortices_max+nconstraints), cavg, cl_design,
     & c(npanels_max*nvortices_max), xle(npanels_max*nvortices_max)

      pi = acos(-1.)

c Form the matrix of influence coefficients
      do 100 i=1,nvortices_tot
        do 200 j=1,nvortices_tot

c First, calculate the effects of the same-side vortices
          yp = (y(i) - y(j))*cos(theta(j)) +
     &         (z(i) - z(j))*sin(theta(j))
          zp = -(y(i) - y(j))*sin(theta(j)) +
     &          (z(i) - z(j))*cos(theta(j))
          r1 = zp**2 + (yp - sp(j))**2
          r2 = zp**2 + (yp + sp(j))**2

          a1 = ((yp - sp(j))/r1 - (yp + sp(j))/r2)*cos(theta(i)
     &     - theta(j)) + (zp/r1 - zp/r2)*sin(theta(i) - theta(j))

c If the configuration is symmetric, change the sign of y(j) and
c theta(j) to account for image vortices
          if (sym_flag .eq. 1) then
            yp = (y(i) + y(j))*cos(-theta(j)) +
     &           (z(i) - z(j))*sin(-theta(j))
            zp = -(y(i) + y(j))*sin(-theta(j)) +
     &            (z(i) - z(j))*cos(-theta(j))
            r1 = zp**2 + (yp - sp(j))**2
            r2 = zp**2 + (yp + sp(j))**2
            a2 = ((yp - sp(j))/r1 - (yp + sp(j))/r2)*cos(theta(i)
     &       + theta(j)) + (zp/r1 - zp/r2)*sin(theta(i) + theta(j))
          else
            a2 = 0.
          end if

c Add the two influences to form the total influence coefficients
          a(i,j) = -cavg/(4.*pi)*(a1 + a2)

  200   continue
  100 continue

c Implement method of Lagrange multipliers
      do 300 i=1,nvortices_tot
        do 400 j=1,nvortices_tot
          abar(i,j) = a(i,j)*s(i) + a(j,i)*s(j)
  400   continue
        b(i) = 0.
  300 continue

c Augment influence coefficient matrix with Cl constraint
      do 500 i=1,nvortices_tot
        abar(i,nvortices_tot+1) = s(i)*cos(theta(i))
  500 continue
      do 600 j=1,nvortices_tot
        abar(nvortices_tot+1,j) = s(j)*cos(theta(j))
  600 continue
      abar(nvortices_tot+1,nvortices_tot+1) = 0.
      if (sym_flag .eq. 1) then
        b(nvortices_tot+1) = cl_design/2.
      else
        b(nvortices_tot+1) = cl_design
      end if

c Augment matrix with Cm constraint if cm_flag = 1
      if (cm_flag .eq. 1) then
        do 700 i=1,nvortices_tot
          abar(i,nvortices_tot+2) = s(i)*cos(theta(i))*
     &     (xcg - (xle(i) + cp*c(i)))/cavg
  700   continue
        do 800 j=1,nvortices_tot
          abar(nvortices_tot+2,j) = s(j)*cos(theta(j))*
     &     (xcg - (xle(j) + cp*c(j)))/cavg
  800   continue
        abar(nvortices_tot+1,nvortices_tot+2) = 0.
        abar(nvortices_tot+2,nvortices_tot+1) = 0.
        abar(nvortices_tot+2,nvortices_tot+2) = 0.
        if (sym_flag .eq. 1) then
          b(nvortices_tot+2) = cm_design/2.
        else
          b(nvortices_tot+2) = cm_design
        end if
      end if

      return
      end


