!$Id:$
      subroutine pltcon(x,ie,ix,ip,u,
     &                  nie,ndm,ndf,nen1,nen0,ic,mc,lc,mmc,label)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Plot of mesh contours: With inter-element smoothing

!      Inputs:
!         x(ndm,*)  - Nodal coordinates of mesh
!         ie(nie,*) - Assembly data for material sets
!         ix(nen1,*)- Element nodal connections
!         ip(*)     - Sorted element order
!         u(*)      - Solution state
!         nie       - Dimension of ie array
!         ndm       - Dimension of x array
!         ndf       - Number dof/node
!         nen1      - Dimension of ix array
!         nen0      - Number nodes on plot face
!         ic        - Component number to plot
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
      include  'iofile.h'
      include  'pbody.h'
      include  'pdata1.h'
      include  'pdata2.h'
      include  'pdata4.h'
      include  'pdata6.h'
      include  'pdatri.h'
      include  'plcon1.h'
      include  'ppers.h'
      include  'prange.h'
      include  'prmptd.h'
      include  'psdat1.h'
      include  'rpdata.h'

      character (len=1) :: y

      logical       :: errck,cont,pinput,label,labl,cinput
      integer       :: nie, ndm, ndf, nen1, nen0, ic, mc, lc, mmc
      integer       :: i, j, n, ma, nc, nnc, nerr, numf,numfac
      integer       :: iel, iuf, iutot, ns, ii, ko, ne,nel, pstyp
      real (kind=8) :: vmx, vmn

      integer       :: ie(nie,*),ix(nen1,*),ip(*), iplt(50), icl(30)
      integer       :: ilq(4),iq9(4,4),iq16(4,9),it6(3,4)
      real (kind=8) :: xl(3,29),xq(3,4),x(ndm,*),u(*),v(29),vc(12),vq(4)

      save

      data      it6 / 1, 4, 6,   4, 2, 5,   4, 5, 6,   6, 5, 3/
      data      iq9 / 1, 5, 9, 8,   5, 2, 6, 9,   8, 9, 7, 4,
     &                9, 6, 3, 7/
      data      iq16/ 1, 5,13,12,   5, 6,14,13,   6, 2, 7,14,
     &               12,13,16,11,  13,14,15,16,  14, 7, 8,15,
     &               11,16,10, 4,  16,15, 9,10,  15, 8, 3, 9/

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
        nerr  = 0
11      if(ior.lt.0) write(*,2001) nc
        nnc = min(8,nc)
        if (prompt .and. .not.defalt) then
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
        if (prompt) then
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

      if(ior.lt.0 .and. prompt .and. .not.defalt ) then
        write(*,2006)
!20      read (*,1000,err=21,end=22) y
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

!     Find max/min of plot variable

      call plopen

      j   = ic
      xmx = x(1,1)
      ymx = x(2,1)
      xmn = x(1,1)
      ymn = x(2,1)
      vmn = u(j)
      vmx = u(j)
      do i = 1,numnp
        xmx = max(x(1,i),xmx)
        ymx = max(x(2,i),ymx)
        xmn = min(x(1,i),xmn)
        ymn = min(x(2,i),ymn)
        vmn = min(vmn,u(j))
        vmx = max(vmx,u(j))
        j   = j + ndf
      end do
      if(xmx.ne.xmn) xmx = 8.2d0/(xmx-xmn)
      if(ymx.ne.ymn) ymx = 8.2d0/(ymx-ymn)
      if(vmn.eq.00d0 .and. vmx.eq.0.0d0) then
        write(iow,2005)
        if(ior.lt.0) write(*,2005)
        return
      endif

!     Open plot and loop through elements

      call pzero(xl,3*max(4,nen))
      ic = max(1,min(ic,ndf))
      psmx  = vmn - 1.
      psmn  = vmx + 1.
      do ne = 1,nfac

!       Get plot order for each element

        n  = ip(ne)

!       Plot active regions: material number: maplt; all if maplt = 0;

        if(n.gt.0) then
          ma = ix(nen1,n)
          pstyp = ie(1,ma)
          if(pstyp.gt.0 .and. ix(nen1-1,n).ge.0 .and.
     &      (maplt.eq.0 .or. ma.eq.maplt)) then
            iel = ie(nie-1,ma) ! iel

!           Determine maximum number of nodes on element

            do i = nen0,1,-1
              if(ix(i,n).ne.0) then
                nel = i
                exit
              endif
            end do ! i
            nel = min(nel,nen)
            if(nel.gt.2 .and. ix(1,n).eq.ix(nel,n)) then
              nel       = nel - 1
            endif

!           Get plot order for each element

            if(ix(nen+7,n).eq.-22) then
              pstyp = 2
            else
              call plftyp(pstyp,nel,iel)
            endif

!           Perspective or standard feap elements

            if(kpers.eq.1 .or. pstyp.gt.0) then

!             VEM 2-d elements
              if(ix(nen+7,n).eq.-22) then
                call vem_compp(ix(nen+8,n), iplt, nel, iuf)
!             Standard FEAP elements
              else
                call pltord(ix(1,n),iel, iuf,iplt)
              endif

!             Set values of vlu(1) and vlu(2)

              iutot  = 0
              vlu(1) = vmx
              vlu(2) = vmn
              do i = 1,iuf
                ns = iplt(i)
                if(ns.le.nel) then
                  ii = ix(iplt(i),n)
                  if(ii.gt.0) then
                    iutot    = iutot + 1
                    xl(1,ns) = x(1,ii)
                    xl(2,ns) = x(2,ii)
                    if(ndm.ge.3) xl(3,ns) = x(3,ii)
                    j      = ndf*(ii-1) + ic
                    v(ns)  = u(j)
                    vlu(1) = min(vlu(1),v(ns))
                    vlu(2) = max(vlu(2),v(ns))

!                   Plot min/max for graphics

                    if(psmn.gt.v(ns)) then
                      psmn    = v(ns)
                      xpsn(1) = xl(1,ns)
                      xpsn(2) = xl(2,ns)
                      xpsn(3) = xl(3,ns)
                    endif
                    if(psmx.lt.v(ns)) then
                      psmx    = v(ns)
                      xpsx(1) = xl(1,ns)
                      xpsx(2) = xl(2,ns)
                      xpsx(3) = xl(3,ns)
                    endif
                  endif
                endif
              end do ! i

!             Plot area contours

              if(iutot.gt.3 .and. pstyp.ge.2) then

!               VEM 2-d elements

                if(ix(nen+7,n).eq.-22) then

                  ko = ix(nen+8,n)
                  call vem_plcon(ko, nc,nel, xl, iplt,icl, v,vc, cont)

!               Linear Triangle

                elseif(nel.eq.3) then
                  if(cont) then
                    call pltecn(xl,v,vc,nc)
                  else
                    call pltcor(3,icl,v,vc,nc)
                    call pltefl(3,icl,xl,v,vc,nc)
                  endif

!               Linear Quadrilateral

                elseif(nel.eq.4) then
                  call pltcor(nel,icl,v,vc,nc)
                  call pltqfl(icl,xl,v,vc,nc,cont)

!               Quadratic Triangle

                elseif(nel.eq.6 .or. nel.eq.7) then
                  do j = 1,4
                    do i = 1,3
                      ii      = it6(i,j)
                      xq(1,i) = xl(1,ii)
                      xq(2,i) = xl(2,ii)
                      xq(3,i) = xl(3,ii)
                      vq(i)   = v(ii)
                    end do ! i
                    xq(1,4) = xq(1,1)
                    xq(2,4) = xq(2,1)
                    xq(3,4) = xq(3,1)
                    vq(4)   = vq(1)
                    if(cont) then
                      call pltecn(xq,vq,vc,nc)
                    else
                      call pltcor(3,ilq,vq,vc,nc)
                      call pltefl(3,ilq,xq,vq,vc,nc)
                    endif
                  end do ! j

!               Quadratic quad

                elseif((nel.eq.8 .and. iuf.ne.16) .or. nel.eq.9) then
                  if(nel.eq.8) then
                    do i = 1,3
                      xl(i,9) = - 0.25d0*(xl(i,1) + xl(i,2)
     &                                  + xl(i,3) + xl(i,4))
     &                          + 0.50d0*(xl(i,5) + xl(i,6)
     &                                  + xl(i,7) + xl(i,8))
                    end do ! i
                    v(9) = - 0.25d0*(v(1) + v(2) + v(3) + v(4))
     &                     + 0.50d0*(v(5) + v(6) + v(7) + v(8))
                    nel = 9
                  else
                    ii = ix(9,n)
                    do i = 1,ndm
                      xl(i,9) = x(i,ii)
                    end do ! i
                    v(9)  = u(ndf*(ii-1) + ic)
                  endif
                  nel = 9
                  call pltcor(nel,icl,v,vc,nc)
                  do j = 1,4
                    do i = 1,4
                      ii     = iq9(i,j)
                      ilq(i) = icl(ii)
                      xq(1,i) = xl(1,ii)
                      xq(2,i) = xl(2,ii)
                      xq(3,i) = xl(3,ii)
                      vq(i)   = v(ii)
                    end do ! i
                    call pltqfl(ilq,xq,vq,vc,nc,cont)
                  end do ! j

!               Cubic quad

                elseif(nel.eq.16) then
                  do j = 13,16
                    ii = ix(j,n)
                    do i = 1,ndm
                      xl(i,j) = x(i,ii)
                    end do ! i
                    v(j) = u(ndf*(ii-1) + ic)
                  end do ! j
                  call pltcor(nel,icl,v,vc,nc)
                  do j = 1,9
                    do i = 1,4
                      ii      = iq16(i,j)
                      ilq(i)  = icl(ii)
                      xq(1,i) = xl(1,ii)
                      xq(2,i) = xl(2,ii)
                      xq(3,i) = xl(3,ii)
                      vq(i)   = v(ii)
                    end do ! i
                    call pltqfl(ilq,xq,vq,vc,nc,cont)
                  end do ! j

!               If all else fails plot a quad

                else
                  call pltcor(nel,icl,v,vc,nc)
                  call pltqfl(icl,xl,v,vc,nc,cont)

                endif

!               Draw border around element

                call pppcol(0,1)
                if(.not.cont .and. ipb.eq.0 ) then
                  call plotl(xl(1,iplt(1)),
     &                       xl(2,iplt(1)),
     &                       xl(3,iplt(1)),3)
                  do i = 2,iuf-1
                    j = iplt(i)
                    if(j.le.nel) then
                      call plotl(xl(1,j),xl(2,j),xl(3,j),2)
                    endif
                  end do ! i
                  call plotl(xl(1,iplt(1)),
     &                       xl(2,iplt(1)),
     &                       xl(3,iplt(1)),2)
                endif

!             Line elements

              else
                if(ix(nel,ne).eq.ix(nel-1,ne)) then
                  nel = nel - 1
                endif

                call pltlfl(nel,xl,v,vc,nc)
              endif

!           User plot types

            elseif(pstyp.lt.0) then

              call ufacelib(pstyp,nel,ipu,numfac)

!             Set values of vlu(1) and vlu(2)

              do numf = 1,numfac
                iutot  = 0
                vlu(1) = vmx
                vlu(2) = vmn
                do i = 1,4
                  ns = ipu(i,numf)
                  if(ns.le.nel) then
                    ii = ix(ns,n)
                    if(ii.gt.0) then
                      iutot    = iutot + 1
                      xl(1,iutot) = x(1,ii)
                      xl(2,iutot) = x(2,ii)
                      if(ndm.ge.3) xl(3,iutot) = x(3,ii)
                      j      = ndf*(ii-1) + ic
                      v(iutot)  = u(j)
                      vlu(1) = min(vlu(1),v(iutot))

!                     Plot min/max for graphics

                      if(psmn.gt.v(iutot)) then
                        psmn    = v(iutot)
                        xpsn(1) = xl(1,iutot)
                        xpsn(2) = xl(2,iutot)
                        xpsn(3) = xl(3,iutot)
                      endif
                      if(psmx.lt.v(iutot)) then
                        psmx    = v(iutot)
                        xpsx(1) = xl(1,iutot)
                        xpsx(2) = xl(2,iutot)
                        xpsx(3) = xl(3,iutot)
                      endif
                    endif
                  endif
                end do ! i
                do i = 1,3
                  xl(i,iutot+1) = xl(i,1)
                end do ! i
                call pltcor(  4,icl,v,vc,nc)
                call pltqfl(icl,xl,v,vc,nc,cont)
              end do ! numf

!             Draw border around element

              call pppcol(0,1)

              iuf = inord(ma)
              if(.not.cont .and. ipb.eq.0 ) then
                call plotl(x(1,ix(ipord(1,ma),n)),
     &                     x(2,ix(ipord(1,ma),n)),
     &                     x(3,ix(ipord(1,ma),n)),3)
                do i = 2,iuf
                  call plotl(x(1,ix(ipord(i,ma),n)),
     &                       x(2,ix(ipord(i,ma),n)),
     &                       x(3,ix(ipord(i,ma),n)),2)
                end do ! i
                call plotl(x(1,ix(ipord(1,ma),n)),
     &                     x(2,ix(ipord(1,ma),n)),
     &                     x(3,ix(ipord(1,ma),n)),2)
              endif
            endif
          endif

        endif

      end do ! ne

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
2000  format('   ------ Contour Values for Plot ------'/(3x,5e15.6))
2001  format(' Input',i3,' Contour Values for Plot - 8 Values/Line')
2002  format(' Input number for first contour label > ',$)
2005  format(' ** ERROR ** No plot - all zero values')
2006  format(' Input values correct? (y or n, c = cancel) > ',$)
2007  format(' ** WARNING ** Initial label reset to fit screen')
2008  format(' Input Min/Max (Default:',1p,e9.2,'/',1p,e9.2,'): >',$)
2009  format(3x,'>',$)

      end subroutine pltcon
