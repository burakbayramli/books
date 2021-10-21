
c
c
c     =====================================================
      subroutine bc2(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,
     &               dx,dy,q,maux,aux,t,dt,mthbc)
c     =====================================================
c
c     # Standard boundary condition choices for claw2,
c
c     # Modified for a general quadrilateral grid in the case mthbc(k)=3:
c     # Solid wall boundary conditions are implemented by negating the
c     # velocity normal to the wall.
c
c     # At each boundary  k = 1 (left),  2 (right),  3 (top), 4 (bottom):
c     #   mthbc(k) =  0  for user-supplied BC's (must be inserted!)
c     #            =  1  for zero-order extrapolation
c     #            =  2  for periodic boundary coniditions
c     #            =  3  for solid walls, assuming this can be implemented
c     #                  by reflecting the data about the boundary and then
c     #                  negating the normal component of the velocity.
c     #                  On a quadrilateral grid we know the normal to each
c     #                  edge and assume this is stored in the aux array:
c
c     #    aux(i,j,1)  = ax
c     #    aux(i,j,2)  = ay   where (ax,ay) is unit normal to left face
c     #    aux(i,j,4)  = bx
c     #    aux(i,j,5)  = by   where (bx,by) is unit normal to bottom face
c     ------------------------------------------------
c
c     # Extend the data from the interior cells (1:mx, 1:my)
c     # to the ghost cells outside the region:
c     #   (i, 1-jbc)   for jbc = 1,mbc,  i = 1-mbc, mx+mbc
c     #   (i, my+jbc)  for jbc = 1,mbc,  i = 1-mbc, mx+mbc
c     #   (1-ibc, j)   for ibc = 1,mbc,  j = 1-mbc, my+mbc
c     #   (mx+ibc, j)  for ibc = 1,mbc,  j = 1-mbc, my+mbc
c
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, *)
      dimension mthbc(4)

c
c
c-------------------------------------------------------
c     # left boundary:
c-------------------------------------------------------
      go to (100,110,120,130) mthbc(1)+1
c
  100 continue
c     # user-specified boundary conditions go here in place of error output
      write(6,*) '*** ERROR *** mthbc(1)=0 and no BCs specified in bc2'
      stop
      go to 199
c
  110 continue
c     # zero-order extrapolation:
      do 115 m=1,meqn
         do 115 ibc=1,mbc
            do 115 j = 1-mbc, my+mbc
               q(1-ibc,j,m) = q(1,j,m)
  115       continue
      go to 199

  120 continue
c     # periodic:  
      do 125 m=1,meqn
         do 125 ibc=1,mbc
            do 125 j = 1-mbc, my+mbc
               q(1-ibc,j,m) = q(mx+1-ibc,j,m)
  125       continue
      go to 199

  130 continue
c     # solid wall 
      do 135 m=1,meqn
         do 135 ibc=1,mbc
            do 135 j = 1-mbc, my+mbc
               q(1-ibc,j,m) = q(ibc,j,m)
  135       continue

c     # negate the normal velocity:
c     # (for a general quadrilateral grid)
c
      do 136 ibc=1,mbc
         do 136 j = 1-mbc, my+mbc
	    alf = aux(1,j,1)
	    beta = aux(1,j,2)
            unorm = alf*q(ibc,j,2) + beta*q(ibc,j,3)
            utang = -beta*q(ibc,j,2) + alf*q(ibc,j,3)
            unorm = -unorm
            q(1-ibc,j,2) = alf*unorm - beta*utang
            q(1-ibc,j,3) = beta*unorm + alf*utang
  136    continue
      go to 199

  199 continue
c
c-------------------------------------------------------
c     # right boundary:
c-------------------------------------------------------
      go to (200,210,220,230) mthbc(2)+1
c
  200 continue
c     # user-specified boundary conditions go here in place of error output
      write(6,*) '*** ERROR *** mthbc(2)=0 and no BCs specified in bc2'
      stop
      go to 299

  210 continue
c     # zero-order extrapolation:
      do 215 m=1,meqn
         do 215 ibc=1,mbc
            do 215 j = 1-mbc, my+mbc
               q(mx+ibc,j,m) = q(mx,j,m)
  215       continue
      go to 299

  220 continue
c     # periodic:  
      do 225 m=1,meqn
         do 225 ibc=1,mbc
            do 225 j = 1-mbc, my+mbc
               q(mx+ibc,j,m) = q(ibc,j,m)
  225       continue
      go to 299

  230 continue
c     # solid wall 
      do 235 m=1,meqn
         do 235 ibc=1,mbc
            do 235 j = 1-mbc, my+mbc
               q(mx+ibc,j,m) = q(mx+1-ibc,j,m)
  235       continue
c     # negate the normal velocity:
c     # (for a general quadrilateral grid)
c
      do 236 ibc=1,mbc
         do 236 j = 1-mbc, my+mbc
	    alf = aux(mx+1,j,1)
	    beta = aux(mx+1,j,2)
            unorm = alf*q(mx+1-ibc,j,2) + beta*q(mx+1-ibc,j,3)
            utang = -beta*q(mx+1-ibc,j,2) + alf*q(mx+1-ibc,j,3)
            unorm = -unorm
            q(mx+ibc,j,2) = alf*unorm - beta*utang
            q(mx+ibc,j,3) = beta*unorm + alf*utang
  236    continue
      go to 299

  299 continue
c
c-------------------------------------------------------
c     # bottom boundary:
c-------------------------------------------------------
      go to (300,310,320,330) mthbc(3)+1
c
  300 continue
c     # user-specified boundary conditions go here in place of error output
      write(6,*) '*** ERROR *** mthbc(3)=0 and no BCs specified in bc2'
      stop
      go to 399
c
  310 continue
c     # zero-order extrapolation:
      do 315 m=1,meqn
         do 315 jbc=1,mbc
            do 315 i = 1-mbc, mx+mbc
               q(i,1-jbc,m) = q(i,1,m)
  315       continue
      go to 399

  320 continue
c     # periodic:  
      do 325 m=1,meqn
         do 325 jbc=1,mbc
            do 325 i = 1-mbc, mx+mbc
               q(i,1-jbc,m) = q(i,my+1-jbc,m)
  325       continue
      go to 399

  330 continue
c     # solid wall 
      do 335 m=1,meqn
         do 335 jbc=1,mbc
            do 335 i = 1-mbc, mx+mbc
               q(i,1-jbc,m) = q(i,jbc,m)
  335       continue
c     # negate the normal velocity:
c     # (for a general quadrilateral grid)
c
      do 336 jbc=1,mbc
         do 336 i = 1-mbc, mx+mbc
	    alf = aux(i,1,4)
	    beta = aux(i,1,5)
            unorm = alf*q(i,jbc,2) + beta*q(i,jbc,3)
            utang = -beta*q(i,jbc,2) + alf*q(i,jbc,3)
            unorm = -unorm
            q(i,1-jbc,2) = alf*unorm - beta*utang
            q(i,1-jbc,3) = beta*unorm + alf*utang
  336    continue
      go to 399

  399 continue
c
c-------------------------------------------------------
c     # top boundary:
c-------------------------------------------------------
      go to (400,410,420,430) mthbc(4)+1
c
  400 continue
         do 405 jbc=1,mbc
            do 405 i = 1-mbc, mx+mbc
               q(i,my+jbc,1) = 1.d0
  405       continue
      go to 499

  410 continue
c     # zero-order extrapolation:
      do 415 m=1,meqn
         do 415 jbc=1,mbc
            do 415 i = 1-mbc, mx+mbc
               q(i,my+jbc,m) = q(i,my,m)
  415       continue
      go to 499

  420 continue
c     # periodic:  
      do 425 m=1,meqn
         do 425 jbc=1,mbc
            do 425 i = 1-mbc, mx+mbc
               q(i,my+jbc,m) = q(i,jbc,m)
  425       continue
      go to 499

  430 continue
c     # solid wall 
      do 435 m=1,meqn
         do 435 jbc=1,mbc
            do 435 i = 1-mbc, mx+mbc
               q(i,my+jbc,m) = q(i,my+1-jbc,m)
  435       continue
c     # negate the normal velocity:
c     # (for a general quadrilateral grid)
c
      do 436 jbc=1,mbc
         do 436 i = 1-mbc, mx+mbc
	    alf = aux(i,my+1,4)
	    beta = aux(i,my+1,5)
            unorm = alf*q(i,my+1-jbc,2) + beta*q(i,my+1-jbc,3)
            utang = -beta*q(i,my+1-jbc,2) + alf*q(i,my+1-jbc,3)
            unorm = -unorm
            q(i,my+jbc,2) = alf*unorm - beta*utang
            q(i,my+jbc,3) = beta*unorm + alf*utang
  436    continue
      go to 499

  499 continue

      return
      end
