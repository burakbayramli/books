!$Id:$
      subroutine pltelc(x,ie,ix,ip,dt,st,vv,
     &                  nie,ndm,ndf,nen1,ic,mc,lc,mmc,label)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Plot of element contours: No inter-element smoothing

!      Inputs:
!         x(ndm,*)  - Nodal coordinates of mesh
!         ie(nie,*) - Assembly data for material sets
!         ix(nen1,*)- Element nodal connections
!         ip(*)     - Sorted element order
!         dt(*)     - Storage for element node projection
!         st(numnp*)- Storage for element node projection
!         vv(nen,*) - Element projected value
!         nie       - Dimension of ie array
!         ndm       - Dimension of x array
!         ndf       - Number dof/node
!         nen1      - Dimension of ix array
!         ic        - Component value to plot
!         mc        - Number of contour lines: < 0 fill; > 0 line
!         lc        - Dimensioning information on component to plot
!         mmc       - Type of plot
!         label     - Flag, put labels on plots if true

!      Outputs:
!         none      - Plot outputs to screen/file
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'comfil.h'
      include  'fdata.h'
      include  'hdatam.h'
      include  'iofile.h'
      include  'pbody.h'
      include  'pdata1.h'
      include  'pdata2.h'
      include  'pdata4.h'
      include  'pdatri.h'
      include  'pointer.h'
      include  'prange.h'
      include  'prmptd.h'
      include  'psdat1.h'
      include  'rpdata.h'

      character (len=1) :: y

      logical       :: tvc(9,9),vflg,errck,cont,cinput
      logical       :: pinput,label,labl
      integer       :: nie,ndm,ndf,nen1,ic,mc,lc,mmc,icolor
      integer       :: i,n,ma,maold,nc,nnc,nerr,nlabi, iplt(30)
      integer       :: iu,iutot,ns,ii,ne
      real (kind=8) :: dx1,vmx,vmn

      integer       :: ie(nie,*),ix(nen1,*),ip(*)
      real (kind=8) :: xl(3,29),x(ndm,*),dt(*),st(numnp,*),vv(nen,*)
      real (kind=8) :: v(29),vc(12),vlu(2)

      save

!     Contour plot routine for elements: lines if mc > 0;
!                                        fills if mc < 0

      cont = .true.
      labl = label
      call pzerol ( tvc , .true. , 81 )
1     if(mc.gt.0) then
        nc    = max(1,min(mc,12))
        nlabi = 0
        dx1   = .024d0/scalef
        vflg  = ipb.eq.0
        nerr=0
11      if(ior.lt.0) write(*,2001) nc
        nnc = min(8,nc)
        if(prompt .and. .not.defalt) then
          if(ior.lt.0) write(*,2009)
          errck = pinput(vc,nnc)
          nerr  = nerr+1
          if(nerr.gt.5) then
            if(ior.lt.0) return
            call plstop(.true.)
          endif
          if(errck) go to 11
        else
          vc(1) = 0.0d0
          vc(2) = 0.0d0
        endif
        if(nc.gt.1 .and. vc(1).eq.0.0d0 .and. vc(2).eq.0.0d0) then
          vc(1)   = rmn +    (rmx - rmn)/(nc+1)
          vc(nc)  = rmn + nc*(rmx - rmn)/(nc+1)
          do i = 2,nc-1
            vc(i) = vc(1) + (vc(nc)-vc(1))*(i-1)/(nc-1)
          end do
        else
          if(nc.gt.8) then
            nnc = min(12,nc) - 8
            if(ior.lt.0) write(*,2009)
            errck = pinput(vc(9),nnc)
            if(errck) go to 11
          endif
        endif
        if(prompt) then
          if(pfr) write(iow,2000) (vc(i),i=1,nc)
          if(ior.lt.0 .and. .not.defalt ) then
            write(*,2000) (vc(i),i=1,nc)
          endif
        endif

!       Input label and color for first contour

13      if(prompt .and. .not.defalt) then
          if (ior.lt.0) write(*,2002)
          errck = pinput(vlu(1),1)
          if(errck) go to 13
          nlabi = max(1,min(int(vlu(1)),12)) - 1
          if(nlabi+nc.gt.12) then
            if(ior.lt.0) write(*,2007)
            nlabi = 12 - nc
          endif
        else
          nlabi = 0
        endif

!     Inputs for filled plots

      else
        cont  = .false.
        nc    = 6
        if(ipb.ge.0) then
15        if(rangfl) then
            vc(1) = rangmn
            vc(2) = rangmx
          elseif(prompt .and. .not.defalt) then
            if(ior.lt.0) write(*,2008) rmn,rmx
            errck = pinput(vc,2)
            if(errck) go to 15
          else
            vc(1) = 0.0d0
            vc(2) = 0.0d0
          endif
          if(vc(1).eq.vc(2)) then
            vc(1) = rmn +      (rmx - rmn)/7.0d0
            vc(6) = rmn + 6.d0*(rmx - rmn)/7.0d0
          else
            vc(6) = vc(2)
          endif
          do i = 2,5
            vc(i) = vc(1) + (vc(6)-vc(1))*(i-1)/5.0d0
          end do
          if(prompt) then
            if(pfr) write(iow,2000) (vc(i),i=1,nc)
            if(ior.lt.0 .and. .not.defalt ) then
              write(*,2000) (vc(i),i=1,nc)
            endif
          endif
        endif
      endif

!     If interactive, offer chance to change inputs

      if(ior.lt.0 .and. prompt .and. .not.defalt) then
        write(*,2006)
!20     read (*,1000,err=21,end=22) y
20      if(.not.cinput()) then
          goto 22
        end if
        y = record(1:1)
        goto  23
!21     call  errclr ('PLTCON')
        call  errclr ('PLTCON')
        goto  20
22      call  endclr ('PLTCON',y)
23      if(y.eq.'c' .or.y.eq.'C') return
        if(y.ne.'y'.and.y.ne.'Y') go to 1
      endif

!     Compute projected max/min values

      vmn = st(1,lc)
      vmx = st(1,lc)
      do i = 1,numnp
        vmn = min(vmn,st(i,lc))
        vmx = max(vmx,st(i,lc))
        st(i,lc) = 0.0d0
        dt(i)    = 0.0d0
      end do

      if(vmn.eq.0.0d0 .and. vmx.eq.0.0d0) then
        write(iow,2005)
        if(ior.lt.0) write(*,2005)
        return
      endif

!     Compute element nodal values

      do n = 1,numel

!       Compute value in element

        ne     = n
        pltmfl = .true.
        call formfe(np(40),np(40),np(40),np(40),
     &             .false.,.false.,.false.,8,ne,ne,1)
        pltmfl = .false.

!       Do projection

        do i = 1,nen
          if(ix(i,n).gt.0) then
            if(dt(ix(i,n)).ne.0.0d0) then
              vv(i,n) = st(ix(i,n),lc)/dt(ix(i,n))
            endif
          endif
        end do
        do i = 1,nen
          if(ix(i,n).gt.0) then
            st(ix(i,n),lc) = 0.0d0
            dt(ix(i,n))    = 0.0d0
          endif
        end do
      end do

!     Open plot and find max/min of plot variable

      call plopen
      xmx = x(1,1)
      ymx = x(2,1)
      xmn = x(1,1)
      ymn = x(2,1)
      do i = 1,numnp
        xmx = max(x(1,i),xmx)
        ymx = max(x(2,i),ymx)
        xmn = min(x(1,i),xmn)
        ymn = min(x(2,i),ymn)
      end do
      if(xmx.ne.xmn) xmx = 8.2d0/(xmx-xmn)
      if(ymx.ne.ymn) ymx = 8.2d0/(ymx-ymn)

!     Loop through elements

      call pzero(xl,3*nen)
      ic = 1
      maold = -1
      psmx  = vmn - 1.
      psmn  = vmx + 1.
      do ne = 1,nfac

!       Set element based on symmetry condition

        n  = ip(ne)

!       If region is active: Plot material number maplt
!                            - all if maplt = 0; ma > 0 active

        if(n.gt.0) then

          ma = ix(nen1,n)
          if((ix(nen1-1,n).ge.0) .and.
     &       (ma.gt.0 .and. maplt.eq.0) .or. ma.eq.maplt) then
            ma = ie(nie-1,ix(nen1,n))

!           Get plot order for each element

            call pltord(ix(1,n),ma, iu,iplt)
            iutot = iu

!           Set values of vlu(1) and vlu(2)

            vlu(1) = vmx
            vlu(2) = vmn
            ns     = 0
            do i = 1,iu
              ii = ix(iplt(i),n)
              if(ii.gt.0) then
                ns = ns + 1
                xl(1,ns) = x(1,ii)
                xl(2,ns) = x(2,ii)
                if(ndm.ge.3) xl(3,ns) = x(3,ii)
                v(ns)  = vv(iplt(i),n)
                vlu(1) = min(vlu(1),v(ns))
                vlu(2) = max(vlu(2),v(ns))

!               Plot min/max for graphics

                if(psmn.gt.v(ns)) then
                  psmn = v(ns)
                  xpsn(1) = xl(1,ns)
                  xpsn(2) = xl(2,ns)
                  xpsn(3) = xl(3,ns)
                endif
                if(psmx.lt.v(ns)) then
                  psmx = v(ns)
                  xpsx(1) = xl(1,ns)
                  xpsx(2) = xl(2,ns)
                  xpsx(3) = xl(3,ns)
                endif
              endif
            end do
            if(ns.gt.3) then
              if(nen.ge.9 .and. iutot.eq.9) then
                if(ix(9,n).gt.0) v(29) = vv(9,n)
              elseif(nen.ge.7 .and. iutot.eq.7) then
                if(ix(7,n).gt.0) v(29) = vv(7,n)
              endif
              call pltris(ic,nc,n,ns,iutot,ndm,ndf,nen,nen1,nlabi,
     &                    icolor,ix,x,xl,v,vc,dx1,vlu(1),vlu(2),tvc,
     &                    cont,vflg)
            else
              call pltlfl(ns,xl,v,vc,nc)
            endif
          endif

        endif ! n > 0

      end do

!     Put on labels

      if(labl) then
        if(cont) then
          call pltctx(vc,lc,nlabi,nc,mmc)
        else
          call pltftx(vc,-mc,mmc)
        endif
        labl = .false.
      end if

!     Formats

!1000  format(a)
2000  format('   ------ Contour Values for Plot ------'/(3x,1p,5e15.6))
2001  format(' Input',i3,' Contour Values for Plot - 8 Values/Line')
2002  format(' Input number for first contour label > ',$)
2005  format(' ** ERROR ** No plot - all zero values')
2006  format(' Input values correct? (y or n, c = cancel) > ',$)
2007  format(' ** WARNING ** Initial label reset to fit screen')
2008  format(' Input Min/Max (Default:',1p,e9.2,'/',1p,e9.2,'): >',$)
2009  format(3x,'>',$)
      end subroutine pltelc
