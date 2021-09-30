c##########################################################
      program testpvm
c##########################################################
c     sample program to test pvm installation
c            (taken from the pvm-manual)
c     nproc - number of processes (or processors) in the configuration
c             mashines used are listed in the file 'hostfile' 
c     itids(10) - task identifiers for max. 10 processes
c     mytid - own task identifier
c     mytid(1) - task identifier of parent process (which started others)
c     me - process(or) numer, ranges from 1 (parent) to nproc
c==========================================================
      parameter (nproc=4)
c
c.....include here the file 'fpvm3.h' - supply correct path
c
      include '/home/local/pvm/include/fpvm3.h'
      dimension itids(10)
c
c.....obtain task identifier for own process(or)
c
      call pvmfmytid(mytid)
      print *,'  mytid = ',mytid
c
c.....obtain parent task identifier if not self parent
c
      call pvmfparent(itids(1))
      if(itids(1).lt.0) then
c
c.......parent starting other processes
c
        itids(1)=mytid
        me=1
        call pvmfspawn('tpp',pvmdefault,'*',nproc-1,itids(2),numt)
        print *,'  parent spawned task'
c
c.......parent broadcasting task identifiers to all children
c
        call pvmfinitsend(0,info)
        call pvmfpack(integer4,itids,nproc,1,info)
        call pvmfmcast(nproc-1,itids(2),0,info)
        print *,'  parent cast itids '
      else
c
c.......children receiving task identifiers from parent
c
        call pvmfrecv(itids(1),0,info)
        call pvmfunpack(integer4,itids,nproc,1,info)
c
c.......children finding which process(or) they are
c
        do i=2,nproc
          if(mytid.eq.itids(i)) me=i
        end do
      endif
c
c.....all processes(ors) performing some work
c
      call dowork(me,itids,nproc)
c
c.....exiting pvm
c
      call pvmfexit(info)
      stop
      end
c
c
c##########################################################
      subroutine dowork(me,itids,nproc)
c##########################################################
c
c.....include here the file 'fpvm3.h' - supply correct path
c
      include '/home/local/pvm/include/fpvm3.h'
      dimension itids(10)
c
      if(me.eq.1) then
c
c.......parent process sends its task identifier to proc. #2
c
        itok=itids(1)
        print *,'  parent sending token',itok
        call pvmfinitsend(0,info)
        call pvmfpack(integer4,itok,1,1,info)
        call pvmfsend(itids(me+1),4,info)
        print *,'  parent sent token'
c
c.......parent receiving token from proc. #nproc (last in the ring)
c
        call pvmfrecv(itids(nproc),4,info)
        call pvmfunpack(integer4,itok,1,1,info)
        print *,'  parent received token',itok
c
      else
c
c.......child receiving token from preceeding process(or)
c
        call pvmfrecv(itids(me-1),4,info)
        call pvmfunpack(integer4,itok,1,1,info)
c
c.......child adds its tid to token and sends it next proc. or parent
c
        itok=itok+itids(me)
        call pvmfinitsend(0,info)
        call pvmfpack(integer4,itok,1,1,info)
        idest=itids(me+1)
        if(me.eq.nproc) idest=itids(1)
        call pvmfsend(idest,4,info)
      endif
      return
      end
 
