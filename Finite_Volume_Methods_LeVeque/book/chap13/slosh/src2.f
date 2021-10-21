c
c      =======================================================
       subroutine src2(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,
     &                 dx,dy,q,maux,aux,t,dt)
c      =======================================================
c
       implicit double precision (a-h,o-z)
       integer maxmx,maxmy,meqn,mbc,mx,my
       dimension    q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
       common /tank/ ampx,ampy,freqx,freqy,phasex,phasey
c
c
       pi=4.0d0*datan(1.0d0)
       xveldt= ampx*freqx*dcos(freqx*(t+dt)+phasex*pi)
       xvel  = ampx*freqx*dcos(freqx*t+phasex*pi)
       yveldt= ampy*freqy*dcos(freqy*(t+dt)+phasey*pi)
       yvel  = ampy*freqy*dcos(freqy*t+phasey*pi)
       do 100 i=1-mbc,mx+mbc
        do 100 j=1-mbc,my+mbc
         q(i,j,1) = q(i,j,1) !dh/dt=0
         q(i,j,2) = q(i,j,2) + q(i,j,1)*(xveldt-xvel)
         q(i,j,3) = q(i,j,3) + q(i,j,1)*(yveldt-yvel)
  100  continue
       return
       end
