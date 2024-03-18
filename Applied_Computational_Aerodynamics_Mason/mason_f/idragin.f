c///////////////////////////////////////////////////////////////////////
c
c  program idragin
c
c    This program reads the design or analysis input file, and calls
c  the idrag subroutine to calculate the performance of the configuration.
c
c  Program Execution
c
c    The user is prompted for the input and output filenames.  Note that
c  the comments in the sample input files are ignored, since the
c  unformatted read statements only use the first value on each line of
c  input.
c
c  Variable Definitions
c
c  cd_induced   induced drag coefficient
c  cl_design    design lift coefficient
c  cm_flag      flag for Cm constraint (0 = no constraint, 1 = constraint)
c  cm_design    design pitching moment coefficent (about the cg)
c  cp           percent chord location of center of pressure for all sections
c  cavg         average chord (cavg = sref/bref)
c  d            distance along span for load distribution
c  header       header which identifies the input and output files
c  i            index
c  infile       input filename
c  input_mode   input mode (0 = design unknown loads, 1 = analyze given loads)
c  j            index
c  k            index
c  loads        vector of loads for analysis mode
c  load_flag    load flag (0 = cn input, 1 = load (cn*c/cavg) input)
c  load_input   input loads specified at given spanwise stations
c  load_station percent span locations of specified loads in the range [0,1]
c  nloads       number of loads specified per panel for analysis mode input
c  npanels      number of panels in geometry
c  npanels_max  maximum number of panels in geometry
c  nvortices    number of vortices per panel
c  nvortices_max  maximum number of vortices per panel
c  outfile      output filename
c  p            linear interpolation parameter
c  spacing_flag vortex spacing flag (0 = equal, 1 = outboard-compressed,
c                2 = inboard-compressed, 3 = end-compressed)
c  sref         reference area
c  stdin        unit number for standard input (5)
c  stdout       unit number for standard output (6)
c  sym_flag     symmetry flag (0 = asymmetric, 1 = symmetric)
c  temp_load    temporary load vector for analysis mode
c  title        title of aircraft configuration
c  unit_read    unit number for read statements
c  write_flag   write flag (0 = no output file, 1 = output file written)
c  xc, yc, zc   coordinates of corner points (aircraft reference frame)
c  xcg          x location of the center of gravity
c
c  Created by:  Joel Grasmeyer
c  Last Modified:  02/10/97
c  Version 1.1
c  Please send any bug reports to grasmeye@aoe.vt.edu
c
c///////////////////////////////////////////////////////////////////////

      program idragin

      integer npanels_max, nvortices_max, stdin, stdout, unit_read
      parameter(npanels_max=5,nvortices_max=200,stdin=5,stdout=6,
     & unit_read=10)

      character*72 infile, outfile, header, title
      integer npanels, nvortices(npanels_max),
     & i, j, input_mode, cm_flag, write_flag, sym_flag,
     & nloads(npanels_max), load_flag, k, spacing_flag(npanels_max)
      real cl_design, sref, cm_design, xcg, cp, cavg, cd_induced,
     & xc(npanels_max,4), yc(npanels_max,4), zc(npanels_max,4),
     & load_station(npanels_max,nvortices_max), d(nvortices_max),
     & load_input(npanels_max,nvortices_max), p(nvortices_max),
     & temp_load(npanels_max,nvortices_max),
     & loads(npanels_max*nvortices_max)

c Get input and output filenames
      write(stdout,*) 'Enter input filename within single quotes: '
      read(stdin,*) infile
      write(stdout,*) 'Enter output filename within single quotes: '
      read(stdin,*) outfile

c Read input that is common to both modes
      open(unit_read,file=infile)
      read(unit_read,101) header
      read(unit_read,101) title
      read(unit_read,*) input_mode
      read(unit_read,*) write_flag
      read(unit_read,*) sym_flag
  101 format(a72)

c Read input file for design mode
      if (input_mode .eq. 0) then
        read(unit_read,*) cl_design
        read(unit_read,*) cm_flag
        read(unit_read,*) cm_design
        read(unit_read,*) xcg
        read(unit_read,*) cp
        read(unit_read,*) sref
        read(unit_read,*) cavg
        read(unit_read,*) npanels
        do 100 i=1,npanels
          do 200 j=1,4
            read(unit_read,*) xc(i,j), yc(i,j), zc(i,j)
  200     continue
          read(unit_read,*) nvortices(i)
          read(unit_read,*) spacing_flag(i)
  100   continue

c Initialize variables that are not used in the design mode
        load_flag = 0
        do 300 i=1,npanels_max*nvortices_max
          loads(i) = 0.
  300   continue

c Read input file for analysis mode
      else
        read(unit_read,*) load_flag
        read(unit_read,*) xcg
        read(unit_read,*) cp
        read(unit_read,*) sref
        read(unit_read,*) cavg
        read(unit_read,*) npanels
        do 400 i=1,npanels
          do 500 j=1,4
            read(unit_read,*) xc(i,j), yc(i,j), zc(i,j)
  500     continue
          read(unit_read,*) nvortices(i)
          read(unit_read,*) spacing_flag(i)
          read(unit_read,*) nloads(i)
          do 600 k=1,nloads(i)
            read(unit_read,*) load_station(i,k), load_input(i,k)
  600     continue
  400   continue

c Initialize variables that are not used in the analysis mode
        cl_design = 0.
        cm_flag = 0
        cm_design = 0.

c Perform linear interpolation on load input
        do 700 i=1,npanels
          do 800 j=1,nvortices(i)
            d(j) = (j - 0.5)/nvortices(i)
  800     continue
          j = 1
          k = 2
 1100     if (j .le. nvortices(i)) then
 1200       if (d(j) .le. load_station(i,k) .and.
     &       j .le. nvortices(i)) then
              p(j) = (d(j) - load_station(i,k-1))/
     &         (load_station(i,k) - load_station(i,k-1))
              temp_load(i,j) = load_input(i,k-1) + p(j)*
     &         (load_input(i,k) - load_input(i,k-1))
              j = j + 1
              go to 1200
            end if
            k = k + 1
            go to 1100
          end if
  700   continue

c Vectorize loads
        k = 0
        do 900 i=1,npanels
          do 1000 j=1,nvortices(i)
            k = k + 1
            loads(k) = temp_load(i,j)
 1000     continue
  900   continue

      end if

c Close input file
      close(10)

c Call drag subroutine to calculate performance
      call idrag(outfile,title,input_mode,write_flag,sym_flag,
     & cl_design,cm_flag,cm_design,xcg,cp,sref,cavg,npanels,xc,yc,zc,
     & nvortices,spacing_flag,load_flag,loads,cd_induced)

      end

