      subroutine setprob
      implicit double precision (a-h,o-z)
      common /param/  grav
      common /tank/ ampx,ampy,freqx,freqy,phasex,phasey
c
c
      open(unit=7,file='setprob.data',status='old',form='formatted')

c
       read(7,*) grav

       read(7,*) ampx, freqx, phasex
       read(7,*) ampy, freqy, phasey
c
c
      return
      end
