!$Id:$
      subroutine filnam

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set up files for input/output, etc.

!      Inputs:
!         None

!      Outputs:
!         File names returned in common /comfil/
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'chdata.h'
      include  'comfil.h'
      include  'iodata.h'
      include  'iofile.h'
      include  'prmptd.h'
      include  'setups.h'
      include  'vdata.h'

      character (len=128) :: dinp,dout,dres,dplt
      character (len=128) :: pinp,pout,pres,psav,pplt,filein
      character (len=6) :: wd(2)
      character (len=1) :: y

      logical      :: pcomp,lfil,linp,lout,lres,lsav,lplt
      logical      :: fruns,errck,cinput
      integer      :: iorsav, nargs, iinp, ioup,ires,isav,iplt, i,j

      save

      data      dinp,dout,dres,dplt/'NONE','NONE','NONE','NONE'/
      data      wd/'New','Exists'/

!     Save input unit number set command line arguments to 0

      iorsav = ior

!     Add extenders or get filenames from command line

      findex = 1
      nargs  = 0
      call filargs(nargs)
      if(nargs.gt.0) then
        fruns = .false.
        go to 200
      else
        fruns = .true.
      endif

!     Look to see if any problem has been run

101   inquire(file='feapname',exist=lfil)
      if(lfil) then
        open(ios,file='feapname',status='old')
        read(ios,2010,err=900,end=900) pinp,pout,pres,psav,pplt
        close(ios)
        finp = pinp
        fout = pout
        fres = pres
        fsav = psav
        fplt = pplt

!       Continue solution for serial runs

        if(nargs.eq.0) then
          ior = -abs(ior)
          write(*,2000) versn
        endif
        go to 200
      else

!       Set default file names

        ior = -abs(ior)
        write(*,2000) versn
        pinp = dinp
        finp = dinp
        pout = dout
        fout = dout
        pres = dres
        fres = dres
        psav = dres
        fsav = dres
        pplt = dplt
        fplt = dplt
      endif

!     Name file for input data

100   finp = pinp

!     Use current filenames

1     if(y.eq.'N' .or. y.eq.'n' .or. y.eq. ' ') then
        write(*,2011)
        write(*,2002) pinp
      else
        write(*,2001)
        write(*,2002) pinp
      endif

!     User input of filenames

      call pprint('                   Enter Name -->')
      errck  = cinput()
      filein = record(1:128)

!     Check for stop

      if (pcomp(filein,'stop',4) .or.
     &    pcomp(filein,'exit',4) .or.
     &    pcomp(filein,'quit',4)) then
        call plstop(.false.)

!     Set active input filename

      elseif (.not.pcomp(filein,' ',1)) then
        finp = ' '
        finp = filein
        do j = len_trim(finp),1,-1
          if(finp(j:j).eq.char(47) .or.       ! char(47) = '/'
     &       finp(j:j).eq.char(92)) go to 110 ! char(92) = '\'
        end do ! j
        j = 0
110     findex = j + 1
      endif

!     Check if input files exists

      inquire(file=finp,exist=linp)

      if(.not.linp) then
        write(*,3000) finp
        go to 1
      endif
      pinp = finp

!     Set default files for a filname beginning with 'I' or 'i'

      if(pcomp(pinp(findex:findex),'I',1)) then
        pinp(findex:findex) = 'I'    ! Force upper case into filename
        pout      = pinp             ! Default output data  file
        pout(findex:findex) = 'O'
        pres      = pinp             ! Default restart read file
        pres(findex:findex) = 'R'
        psav      = pinp             ! Default restart save file
        psav(findex:findex) = 'R'
        pplt      = pinp             ! Default plot file
        pplt(findex:findex) = 'P'
      endif

!     Set default filenames from previous executions

      fout = pout
      fres = pres
      fsav = psav
      fplt = pplt

!     Accept current files unless reset requested

      if(y.ne.'R' .and. y.ne.'r') then
        go to 200
      endif

!     Name file for output data

      write(*,2003) pout
      call pprint('                   Enter Name -->')
      errck  = cinput()
      filein = record(1:128)
      if (.not.pcomp(filein,' ',1)) fout = filein
      pout = fout

!     Name file for restart read data

      write(*,2004) pres
      call pprint('                   Enter Name -->')
      errck  = cinput()
      filein = record(1:128)
      if (.not.pcomp(filein,' ',1)) fres = filein
      pres = fres

!     Name file for restart save data

      write(*,2005) psav
      call pprint('                   Enter Name -->')
      errck  = cinput()
      filein = record(1:128)
      if (.not.pcomp(filein,' ',1)) fsav = filein
      psav = fsav

!     Name file for plot data

      write(*,2006) pplt
      call pprint('                   Enter Name -->')
      errck  = cinput()
      filein = record(1:128)
      if (.not.pcomp(filein,' ',1)) fplt = filein
      pplt = fplt

!     Check file status and input if necessary

200   inquire(file=finp,exist=linp)
      if(.not.linp.and.nargs.gt.0) call plstop(.true.)
      if(.not.linp) go to 100
      iinp = 2
      inquire(file=fout,exist=lout)
      ioup = 1
      if(lout) ioup = 2
      inquire(file=fres,exist=lres)
      ires = 1
      if(lres) ires = 2
      inquire(file=fsav,exist=lsav)
      isav = 1
      if(lsav) isav = 2
      inquire(file=fplt,exist=lplt)
      iplt = 1
      if(lplt) iplt = 2
      if(nargs.gt.0) go to 300
      write(*,2007) wd(iinp),finp,wd(ioup),fout,wd(ires),fres,
     &              wd(isav),fsav,wd(iplt),fplt

!     Start execution

      xxx = ' '
      write(xxx,2008)
      call pprint(xxx)
      errck = cinput()
      y     = record(1:1)
      if(y.eq.'S' .or. y.eq.'s') call plstop(.false.)
      if(y.ne.'Y' .and. y.ne.'y') go to 100

!     Save a copy of current filenames

300   open(ios,file='feapname',status='unknown')
      rewind ios
      write(ios,2010) finp,fout,fres,fsav,fplt
      close(ios)
      if(fruns) then
        write(*,2009)
        if(rank.eq.0 .and. ntasks.gt.1) then
          write(*,2012) ntasks
        endif
      endif

!     Set final findex
      i = index(finp,' ')
      if(i.eq.0) i = 128
      do j = i,1,-1
        if(pcomp(finp(j:j),char(47),1) .or.       ! char(47) = '/'
     &     pcomp(finp(j:j),char(92),1)) go to 310 ! char(92) = '\'
      end do ! j
      j = 0
310   findex = j + 1

!     Reset ior unit number

      ior = iorsav
      return

!     Error in form of 'feapname' file

900   lfil = .false.
      write(*,'(/a)') ' ERROR IN feapname FILE -- Read filenames'
      close(ios,status = 'delete')
      go to 101

!     Formats

2000  format(//
     & '    F I N I T E   E L E M E N T   A N A L Y S I S',
     & '   P R O G R A M'
     &   /14x,'FEAPpv (P e r s o n a l   V e r s i o n)',
     &  //11x,'FEAPpv(C) Regents of the University of California'
     &   /25x,'All Rights Reserved.'/23x,'VERSION: ',a/26x,'DATE: ',a)

2001  format(/8x,' I n p u t    F i l e n a m e s',
     & //8x,' Specify filenames:'/)
2002  format(11x,'Input   Data (default: ',a32,') :')
2003  format(11x,'Output  Data (default: ',a32,') :')
2004  format(11x,'Restart Read (default: ',a32,') :')
2005  format(11x,'Restart Save (default: ',a32,') :')
2006  format(11x,'Plot File    (default: ',a32,') :')

2007  format(/8x,' Files are set as:   Status  Filename'//
     &       11x,'Input   (read ) : ',a6,2x,a32/
     &       11x,'Output  (write) : ',a6,2x,a32/
     &       11x,'Restart (read ) : ',a6,2x,a32/
     &       11x,'Restart (write) : ',a6,2x,a32/
     &       11x,'Plots   (write) : ',a6,2x,a32//
     &  8x,' Caution, existing write files will be overwritten.'/)

2008  format( 8x,' Are filenames correct?',
     &     '( y or n; r = redefine all; s = stop) :')

2009  format(/8x,' R U N N I N G    F E A P p v    P R O B L E M',
     &       '    N O W')

2010  format(a/a/a/a/a)

2011  format(/8x,'Specify input data filename:'/)

2012  format(/8x,' Parallel Solution: Total Number of Tasks =',i5)

3000  format(/' *ERROR* FILNAM: Specified input file: ',a/
     &        '         does not exist. Reinput filename.'/)

      end subroutine filnam
