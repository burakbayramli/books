!$Id:$
      subroutine outary(array,ct)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Output contents of named "array"

!      Inputs:
!         array     - name of array to print
!         ct(3)     - range of array to print

!      Outputs:
!         none      - To screen/file
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'allotn.h'
      include  'allotd.h'
      include  'cdata.h'
      include  'cdat1.h'
      include  'comblk.h'
      include  'iofile.h'
      include  'pointer.h'
      include  'p_point.h'
      include  'sdata.h'

      character     :: array*(*)
      logical       :: cknon0, ckinon0
      logical       :: pcomp, norange
      integer       :: ckmstrt, lengt, i,ilor,iupr,ict(3), ipa,irp(2,2)
      integer       :: nn,nterm,narry, nsz
      real (kind=8) :: ct(3)

      save

      data      irp  / 3*1, 2 /

!     Output dictionary array values

      do i = 1,3
        ict(i) = nint(ct(i))
      end do ! i

!     Set range of print

      ilor    = max(0,min(ict(1),ict(2)))
      iupr    = max(ict(1),ict(2))
      norange = ilor.eq.0 .and. iupr.eq.0

      do i = 1,ndict
        if(pcomp(array,dict(i),5)) then

!         Assign pointer, length, and precision

          ipa   = irp(iprec(i),ipr)
          lengt = ipoint(i)

!         Set range

          if(iupr.eq.0) then
            iupr = lengt - ilor
          else
            ilor = min(iupr,ilor)  - 1
            iupr = min(iupr,lengt) - ilor
          endif

!         Set final pointer

          point = np(dlist(i)) + ilor
          nterm = 0

!         Output formatted array values

          if(norange) then
            if(    pcomp(array,'F    ',5) .or.
     &             pcomp(array,'F0   ',5) .or.
     &             pcomp(array,'FTN  ',5) .or.
     &             pcomp(array,'U    ',5) .or.
     &             pcomp(array,'VEL  ',5)) then
              narry = (iupr-1)/(ndf*numnp) + 1
              do nn = 1,narry
                if(cknon0(hr(point),ndf*numnp)) then
                  call mprint(hr(point),ndf,numnp,ndf,dict(i))
                else
                  write(iow,2000) nn,dict(i)
                  if(ior.lt.0) then
                    write(*,2000) nn,dict(i)
                  endif
                endif
                point = point + ndf*numnp
                nterm = nterm + ndf*numnp
              end do ! nn

!           Output global nodal coordinate array

            elseif(pcomp(array,'X    ',5)) then
              narry = (iupr-1)/(ndm*numnp) + 1
              do nn = 1,narry
                if(cknon0(hr(point),ndm*numnp)) then
                  call mprint(hr(point),ndm,numnp,ndm,dict(i))
                else
                  write(iow,2000) nn,dict(i)
                  if(ior.lt.0) then
                    write(*,2000) nn,dict(i)
                  endif
                endif
                point = point + ndm*numnp
                nterm = nterm + ndm*numnp
              end do ! nn

!           Output nodal FPRO or ID array

            elseif(pcomp(array,'FPRO ',5) .or.
     &             pcomp(array,'ID   ',5)) then
              narry = (iupr-1)/(ndf*numnp) + 1
              do nn = 1,narry
                if(ckinon0(mr(point),ndf*numnp)) then
                  call iprint(mr(point),ndf,numnp,ndf,dict(i))
                else
                  write(iow,2000) nn,dict(i)
                  if(ior.lt.0) then
                    write(*,2000) nn,dict(i)
                  endif
                endif
                point = point + ndf*numnp
                nterm = nterm + ndf*numnp
              end do ! nn

!           Output material IE array

            elseif(pcomp(array,'IE   ',5)) then
              if(ckinon0(mr(point),nie*nummat)) then
                call iprint(mr(point),nie,nummat,nie,dict(i))
              else
                write(iow,2000) 1 ,dict(i)
                if(ior.lt.0) then
                  write(*,2000) 1 ,dict(i)
                endif
              endif
              point = point + nie*nummat
              nterm = nterm + nie*nummat

!           Output material IEDOF array

            elseif(pcomp(array,'IEDOF',5)) then
              do nn = 1,nummat
                if(ckinon0(mr(point),ndf*nen)) then
                  call iprint(mr(point),ndf,nen,ndf,dict(i))
                else
                  write(iow,2000) nn,dict(i)
                  if(ior.lt.0) then
                    write(*,2000) nn,dict(i)
                  endif
                endif
                point = point + ndf*nen
                nterm = nterm + ndf*nen
              end do ! nn

!           Output global element IX connection and control array

            elseif(pcomp(array,'IX   ',5)) then
              if(ckinon0(mr(point),nen1*numel)) then
                call iprint(mr(point),nen1,numel,nen1,dict(i))
              else
                write(iow,2000) 1 ,dict(i)
                  if(ior.lt.0) then
                    write(*,2000) 1 ,dict(i)
                  endif
              endif
              point = point + nen1*numel
              nterm = nterm + nen1*numel

!           Output element LD element assembly array

            elseif(pcomp(array,'LD   ',5)) then
              narry = (iupr-1)/nst + 1
              do nn = 1,narry
                if(ckinon0(mr(point),ndf*nen)) then
                  call iprint(mr(point),ndf,nen,ndf,dict(i))
                else
                  write(iow,2000) nn,dict(i)
                  if(ior.lt.0) then
                    write(*,2000) nn,dict(i)
                  endif
                endif
                point = point + ndf*nen
                nterm = nterm + ndf*nen
              end do ! nn

!           Output element P array

            elseif(pcomp(array,'P    ',5)) then
              narry = (iupr-1)/nst + 1
              do nn = 1,narry
                nsz = (ckmstrt(hr(point),ndf*nen) + ndf - 1)/ndf
                if(nsz.gt.0) then
                  call mprint(hr(point),ndf,nsz,ndf,dict(i))
                else
                  write(iow,2000) nn,dict(i)
                  if(ior.lt.0) then
                    write(*,2000) nn,dict(i)
                  endif
                endif
                point = point + ndf*nen
                nterm = nterm + ndf*nen
              end do ! nn

!           Output element S array

            elseif(pcomp(array,'S    ',5)) then
              narry = (iupr-1)/(nst*nst) + 1
              do nn = 1,narry
                nsz = (ckmstrt(hr(point),nst*nst) + nst - 1)/nst
                if(nsz.gt.0) then
                  call mprint(hr(point),nsz,nsz,nst,dict(i))
                else
                write(iow,2000) nn,dict(i)
                  if(ior.lt.0) then
                    write(*,2000) nn,dict(i)
                  endif
                endif
                point = point + nst*nst
                nterm = nterm + nst*nst
              end do ! nn

!           Output local nodal solution UL array

            elseif(pcomp(array,'UL   ',5)) then
              narry = (iupr-1)/(ndf*nen) + 1
              do nn = 1,narry
                if(cknon0(hr(point),ndf*nen)) then
                  call mprint(hr(point),ndf,nen,ndf,dict(i))
                else
                  write(iow,2000) nn,dict(i)
                  if(ior.lt.0) then
                    write(*,2000) nn,dict(i)
                  endif
                endif
                point = point + ndf*nen
                nterm = nterm + ndf*nen
              end do ! nn

!           Output local element coordinate XL array

            elseif(pcomp(array,'XL   ',5)) then
              narry = (iupr-1)/(3*nen) + 1
              do nn = 1,narry
                if(cknon0(hr(point),ndm*nen)) then
                  call mprint(hr(point),ndm,nen,ndm,dict(i))
                else
                  write(iow,2000) nn,dict(i)
                  if(ior.lt.0) then
                    write(*,2000) nn,dict(i)
                  endif
                endif
                point = point + ndm*nen
                nterm = nterm + ndm*nen
              end do ! nn

!           Output nodal NORMV normal vector array

            elseif(pcomp(array,'NORMV',5)) then
              if(ckinon0(mr(point),3*numnp)) then
                call mprint(hr(point),3,numnp,3,dict(i))
              else
                write(iow,2000) 1 ,dict(i)
                  if(ior.lt.0) then
                    write(*,2000) 1 ,dict(i)
                  endif
              endif
              point = point + 3*numnp
              nterm = nterm + 3*numnp

            endif

!         Output other array values in unformatted form

          else

            if(iprec(i).eq.1) then
              if(ckinon0(mr(point),iupr)) then
                call iprint(mr(point),1,iupr,1,dict(i))
              else
                write(iow,2001) dict(i)
                if(ior.lt.0) then
                  write(*,2001) dict(i)
                endif
              endif
            else
              if(cknon0(hr(point),iupr)) then
                call mprint(hr(point),1,iupr,1,dict(i))
              else
                write(iow,2001) dict(i)
                if(ior.lt.0) then
                  write(*,2001) dict(i)
                endif
              endif
            endif
            point = point + iupr
            nterm = nterm + iupr
          endif

!         Write remainder of array if necessary

          if(nterm.lt.iupr) then
            if(iprec(i).eq.1) then
              if(ckinon0(mr(point),iupr-nterm)) then
                call iprint(mr(point),1,iupr-nterm,1,dict(i))
              else
                write(iow,2000) 1 ,dict(i)
                if(ior.lt.0) then
                  write(*,2000) 1 ,dict(i)
                endif
              endif
            else
              if(cknon0(hr(point),iupr-nterm)) then
                call mprint(hr(point),1,iupr-nterm,1,dict(i))
              else
                write(iow,2000) 1 ,dict(i)
                if(ior.lt.0) then
                  write(*,2000) 1 ,dict(i)
                endif
              endif
            endif
          endif

        endif
      end do ! i

!     Format

2000  format(' --> Part',i3,' of array ',a,' is zero')
2001  format(' --> Terms in array ',a,' are zero')

      end subroutine outary

      integer function ckmstrt(v,nn)

!     Purpose: Find last non-zero entry in array

      implicit   none

      integer       :: nn,n
      real (kind=8) :: v(nn)

!     Find non-zero entry

      do n = nn,1,-1
        if(v(n).ne.0.0d0) then
          ckmstrt = n
          return
        endif
      end do ! n

      ckmstrt = 0

      end function ckmstrt
