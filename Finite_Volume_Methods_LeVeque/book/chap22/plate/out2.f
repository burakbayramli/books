c
c
c =========================================================
      subroutine out2(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,
     &                 dx,dy,q,t,iframe,aux,maux)
c =========================================================
c
c     # Output the results for a general system of conservation laws
c     # in 2 dimensions
c
c     # Write the results to the file fort.q<iframe>
c     # Use format required by matlab script  plotclaw2.m
c     # The same format is used by the amrclaw package.
c     # Here it's adapted to output just the single grid.
c
c     # set outaux = .true. to also output the aux arrays to fort.a<iframe>
c
c     # This copy modified to only output aux(..6) and aux(..7), the
c     # two components of the aux array that hold the displacements.
c
c
      implicit double precision (a-h,o-z)
      dimension   q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, maux)
      character*10 fname1, fname2, fname3
      logical outaux

      outaux = .true.
c
c     # first create the file name and open file
c
         fname1 = 'fort.qxxxx'
         fname2 = 'fort.txxxx'
         fname3 = 'fort.axxxx'
         nstp = iframe
         do 55 ipos = 10, 7, -1
            idigit = mod(nstp,10)
            fname1(ipos:ipos) = char(ichar('0') + idigit)
            fname2(ipos:ipos) = char(ichar('0') + idigit)
            fname3(ipos:ipos) = char(ichar('0') + idigit)
            nstp = nstp / 10
 55      continue

         open(unit=50,file=fname1,status='unknown',form='formatted')
         open(unit=60,file=fname2,status='unknown',form='formatted')

c
c     # the following parameters are used in amrclaw where there are
c     # multiple grids.  Here they are all set to 1:
      ngrids = 1
      mptr = 1
      level = 1

      write(50,1001) mptr,level,mx,my
 1001 format(i5,'                 grid_number',/,
     &       i5,'                 AMR_level',/,
     &       i5,'                 mx',/,
     &       i5,'                 my')

      write(50,1002) xlower,ylower,dx,dy
 1002 format(e26.16,'    xlow', /,
     &       e26.16,'    ylow', /,
     &       e26.16,'    dx', /,
     &       e26.16,'    dy',/)
c
      do 20 j=1,my
        do 10 i=1,mx
          do m=1,meqn
c            # exponents with more than 2 digits cause problems reading
c            # into matlab... reset tiny values to zero:
             if (dabs(q(i,j,m)) .lt. 1d-99) q(i,j,m) = 0.d0
             enddo
c
          write(50,1005) (q(i,j,m), m=1,meqn)
 1005     format(4e26.16)
c
 10       continue
        write(50,*) ' '
 20     continue
      write(50,*) ' '

      if (outaux) then 
c     # also output the aux arrays:
      open(unit=70,file=fname3,status='unknown',form='formatted')
      write(70,1001) mptr,level,mx,my
      write(70,1002) xlower,ylower,dx,dy
      do 120 j=1,my
         do 110 i=1,mx
            do m=1,maux
c              # exponents with more than 2 digits cause problems reading
c              # into matlab... reset tiny values to zero:
               if (dabs(aux(i,j,m)) .lt. 1d-99) aux(i,j,m) = 0.d0
            enddo
c
c           write(70,1005) (aux(i,j,m), m=1,maux)
            write(70,1005) (aux(i,j,m), m=6,7)
c
  110       continue
         write(70,*) ' '
  120    continue
      write(70,*) ' '
      close(unit=70)
      endif

      maux2 = 2
      write(60,1000) t,meqn,ngrids,maux2

 1000 format(e26.16,'    time', /,
     &       i5,'                 meqn'/,
     &       i5,'                 ngrids'/,
     &       i5,'                 maux'/,/)
c
      close(unit=50)
      close(unit=60)

      return
      end
