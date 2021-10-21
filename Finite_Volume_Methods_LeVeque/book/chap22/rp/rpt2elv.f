c
c
c     =====================================================
      subroutine rpt2(ixy,maxm,meqn,mwaves,mbc,mx,ql,qr,
     &			 aux1,aux2,aux3,imp,asdq,bmasdq,bpasdq)
c     =====================================================
      implicit double precision (a-h,o-z)
c
c     # Riemann solver in the transverse direction for the elastic equations
c     # with varying material properties 
c
c
c     # Contents of ql and qr:
c     # 
c     # q(:,1) = sigma^{11} if ixy=1   or   sigma^{22} if ixy=2
c     # q(:,2) = sigma^{22} if ixy=1   or   sigma^{11} if ixy=2
c     # q(:,3) = sigma^{12} = sigma^{21}
c     # q(:,4) = u          if ixy=1   or   v          if ixy=2
c     # q(:,5) = v          if ixy=1   or   u          if ixy=2
c     # 
c     # auxN holds corresponding slices of the aux array:
c     #  N = 1 for row below
c     #      2 for this row
c     #      3 for row above
c     # 
c     #  auxN(i,1) = rho 
c     #  auxN(i,2) = lambda 
c     #  auxN(i,3) = mu
c     #  auxN(i,4) = cp 
c     #  auxN(i,5) = cs
c
c
c
c     # Split asdq into down-going flux bmasdq and up-going flux bpasdq.
c
c     # imp=1  means  asdq=amdq,    imp=2 means asdq=apdq
c
      dimension    ql(1-mbc:maxm+mbc, meqn)
      dimension    qr(1-mbc:maxm+mbc, meqn)
      dimension    asdq(1-mbc:maxm+mbc, meqn)
      dimension bmasdq(1-mbc:maxm+mbc, meqn)
      dimension bpasdq(1-mbc:maxm+mbc, meqn)
      dimension   aux1(1-mbc:maxm+mbc, *)
      dimension   aux2(1-mbc:maxm+mbc, *)
      dimension   aux3(1-mbc:maxm+mbc, *)
c
c
c
c     # set ku to point to  the component of the system that corresponds
c     # to velocity in the direction of this slice, kv to the orthogonal
c     # velocity.  Similarly ksig11 and ksig22 point to normal stresses.
c     # 3rd component is always shear stress sig12.
c
c
      if (ixy.eq.1) then
         ksig11 = 1
         ksig22 = 2
         ku = 4
         kv = 5
	else
         ksig11 = 2
         ksig22 = 1
         ku = 5
         kv = 4
	endif
c
c
      do 20 i = 2-mbc, mx+mbc
c
c        # imp is used to flag whether wave is going to left or right,
c        # since material properties are different on the two sides
c
         if (imp.eq.1) then 
c            # asdq = amdq, moving to left
	     i1 = i-1
	   else
c            # asdq = apdq, moving to right
	     i1 = i
	   endif
c
c        # The flux difference asdq is split into downward moving parts
c        # traveling at speeds -cp and -cs relative to the medium below and
c        # upward moving parts traveling
c        # at speeds +cp and +cs relative to the medium above.
c
c        # Note that the sum of these parts does not give all of asdq
c        # since there is also reflection at the interfaces which decreases
c        # the flux.
c
c        # jumps in asdq:
         dsig11 = asdq(i,ksig11)
         dsig22 = asdq(i,ksig22)
         dsig12 = asdq(i,3)
         du     = asdq(i,ku)
         dv     = asdq(i,kv)
c
c
c        # Material parameters in each row of cells:
         alamm = aux1(i1,2)
         alam  = aux2(i1,2)
         alamp = aux3(i1,2)
         amum  = aux1(i1,3)
         amu   = aux2(i1,3)
         amup  = aux3(i1,3)
         bulkm = alamm + 2.d0*amum
         bulk  = alam  + 2.d0*amu 
         bulkp = alamp + 2.d0*amup
     
c        # P-wave and S-wave speeds in each row of cells:
	 cpm = aux1(i1,4)
	 cp  = aux2(i1,4)
	 cpp = aux3(i1,4)
	 csm = aux1(i1,5)
	 cs  = aux2(i1,5)
	 csp = aux3(i1,5)
c

c        # transmitted part of down-going P-wave:
         det = bulkm*cp + bulk*cpm
         if (det .eq. 0.d0) then
            write(6,*) 'det1 = 0 in rpt2'
            stop
            endif
	 a1 = (cp*dsig22 + bulk*dv) / det

c        # transmitted part of up-going P-wave:
         det = bulk*cpp + bulkp*cp
         if (det .eq. 0.d0) then
            write(6,*) 'det2 = 0 in rpt2'
            stop
            endif
	 a2 = (cp*dsig22 - bulk*dv) / det
c
c        # transmitted part of down-going S-wave:
         det = -(amum*cs + amu*csm)
         if (det .eq. 0.d0) then
             a3 = 0.d0
           else
	     a3 = (cs*dsig12 + amu*du) / det
           endif

c        # transmitted part of up-going S-wave:
         det = -(amu*csp + amup*cs)
         if (det .eq. 0.d0) then
             a4 = 0.d0
           else
	     a4 = (cs*dsig12 - amu*du) / det
           endif
c
c        # The down-going flux difference bmasdq is the product  -c * wave
c        # summed over down-going P-wave and S-wave:
c
         bmasdq(i,ksig11) = -cpm*a1*alamm
         bmasdq(i,ksig22) = -cpm*a1*bulkm
         bmasdq(i,3) =      -csm*a3*(-amum)
         bmasdq(i,ku) =     -csm*a3*(-csm)
         bmasdq(i,kv) =     -cpm*a1*(cpm)
c
c        # The up-going flux difference bpasdq is the product  c * wave
c        # summed over up-going P-wave and S-wave:
c
         bpasdq(i,ksig11) =  cpp*a2*alamp
         bpasdq(i,ksig22) =  cpp*a2*bulkp
         bpasdq(i,3) =       csp*a4*(-amup)
         bpasdq(i,ku) =      csp*a4*(csp)
         bpasdq(i,kv) =      cpp*a2*(-cpp)
c
   20    continue
c
      return
      end
