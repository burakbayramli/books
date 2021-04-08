!$Id:$
      subroutine vem_plcon(k, nc, nel, xl, iplt, icl, v, vc, cont)

      implicit   none

      logical       :: cont
      integer       :: k, nc,nel
      integer       :: iplt(*), icl(*)
      real (kind=8) :: xl(3,*), v(*), vc(*)

      write(*,*) ' This option is available only in the VEM version'

      end subroutine vem_plcon
