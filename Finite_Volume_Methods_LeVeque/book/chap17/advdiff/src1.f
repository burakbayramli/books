
c
c
c =========================================================
      subroutine src1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,dt)
c =========================================================
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
c
c
c     # solve the diffusion equation q_t = q_{xx} using Crank-Nicolson
c     # The LAPACK tridiagonal solver dgtsv is used, which is in tridiag.f
c
c
c     # local storage:
      parameter (maxldb = 5000)
      dimension b(maxldb,1), d(maxldb), dl(maxldb), du(maxldb)
      common /comsrc/ dcoef

      if (mx .gt. maxldb) then
          write(6,*) 'ERROR:  increase maxldb in src1.f'
          endif

      ldb = maxldb
      nrhs = 1
      dtdx2 = dcoef * dt / (2.d0*dx*dx) 
c     
c     # set coefficients in tridiagonal matrix and RHS:
      do i=1,mx
        dl(i) = -dtdx2
        d(i) = 1.d0 + 2.d0*dtdx2
        du(i) = -dtdx2
        b(i,1) = q(i,1) + dtdx2 * (q(i-1,1) - 2.d0*q(i,1) + q(i+1,1))
        enddo
c
c     # no-flux boundary conditions for diffusion step:
c     # Adjust matrix entries to use q(0,1)=q(1,1) and q(mx+1,1)=q(mx,1)
c     # at end of time step:
      d(1) = d(1) - dtdx2
      d(mx) = d(mx) - dtdx2
      
      
c     # to instead set boundary values to specific values, 
c     # comment out the above changes to d(1) and d(mx) and instead 
c     # modify the right-hand side:
c     q0 = 2.d0
c     qmx1 = 0.d0
c     b(1,1) = b(1,1) + dtdx2*q0
c     b(mx,1) = b(mx,1) + dtdx2*qmx1

c
c     # solve the tridiagonal system:
      call dgtsv(mx,nrhs,dl,d,du,b,ldb,info)

      if (info .ne. 0) then
         write(6,*) 'ERROR in src1 from call to dgtsv... info = ',info
         stop
         endif

      do i=1,mx
         q(i,1) = b(i,1)
	 enddo
c
      return
      end
