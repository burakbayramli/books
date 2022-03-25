SUBROUTINE p96(input_file,output_file)
!-------------------------------------------------------------------------
! Program 9.6 Plane strain consolidation analysis of a Biot elastic-plastic
!             (Mohr-Coulomb) material using 8-node rectangular
!             quadrilaterals for displacements coupled to 4-node
!             rectangular quadrilaterals for pressures.  
!             Freedoms numbered in order u-v-uw. Viscoplastic strain method.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,iel,iters,j,k,limit,loaded_nodes,ndim=2,ndof=16,nels,neq,     &
   nip=4,nlen,nlfp,nls,nn,nod=8,nodf=4,nodof=3,npri,nprops=7,np_types,    &
   nr,nres,nst=4,nstep,ntot=20,nxe,nye   
 REAL(iwp)::coh,cons,ddt,det,dpore,dq1,dq2,dq3,dsbar,dt,dtim,d4=4.0_iwp,  &
   d180=180.0_iwp,e,f,lode_theta,one=1.0_iwp,phi,pi,psi,sigm,snph,        &
   start_dt=1.e15_iwp,theta,time,tol,tot_load,two=2.0_iwp,v,zero=0.0_iwp 
 CHARACTER(LEN=15)::argv,element='quadrilateral'
 LOGICAL::converged    
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::nf(:,:),g(:),num(:),g_num(:,:),g_g(:,:),etype(:),   &
   kdiag(:),no(:) 
 REAL(iwp),ALLOCATABLE::al(:),ans(:),bee(:,:),bdylds(:),bload(:),c(:,:),  &
   coord(:,:),dee(:,:),der(:,:),derf(:,:),deriv(:,:),derivf(:,:),devp(:), &
   disps(:),eld(:),eload(:),eps(:),erate(:),evp(:),evpt(:,:,:),flow(:,:), &
   funf(:),g_coord(:,:),jac(:,:),kay(:,:),ke(:,:),km(:,:),kc(:,:),kv(:),  &
   lf(:,:),loads(:),m1(:,:),m2(:,:),m3(:,:),newdis(:),oldis(:),phi0(:),   &
   phi1(:),points(:,:),prop(:,:),sigma(:),store_kc(:,:,:),stress(:),      &
   tensor(:,:,:),val(:,:),vol(:),volf(:,:),weights(:),x_coords(:),        &
   y_coords(:)
!-----------------------input and initialisation--------------------------
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ALLOCATE(dee(nst,nst),points(nip,ndim),coord(nod,ndim),derivf(ndim,nodf),&
   jac(ndim,ndim),kay(ndim,ndim),der(ndim,nod),deriv(ndim,nod),           &
   derf(ndim,nodf),funf(nodf),bee(nst,ndof),km(ndof,ndof),eld(ndof),      &
   sigma(nst),kc(nodf,nodf),g_g(ntot,nels),ke(ntot,ntot),c(ndof,nodf),    &
   x_coords(nxe+1),phi0(nodf),y_coords(nye+1),vol(ndof),nf(nodof,nn),     &
   volf(ndof,nodf),g_coord(ndim,nn),g_num(nod,nels),num(nod),weights(nip),&
   phi1(nodf),store_kc(nodf,nodf,nels),tensor(nst+1,nip,nels),eps(nst),   &
   evp(nst),evpt(nst,nip,nels),bload(ndof),eload(ndof),erate(nst),g(ntot),&
   devp(nst),m1(nst,nst),m2(nst,nst),m3(nst,nst),flow(nst,nst),           &
   stress(nst),etype(nels),prop(nprops,np_types))
 READ(10,*)prop,cons
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)x_coords,y_coords,dtim,nstep,theta,npri,nres
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)
 ALLOCATE(kdiag(neq),loads(0:neq),ans(0:neq),bdylds(0:neq),disps(0:neq),  &
   newdis(0:neq),oldis(0:neq)) 
 READ(10,*)loaded_nodes
 ALLOCATE(no(loaded_nodes),val(loaded_nodes,ndim))
 READ(10,*)(no(i),val(i,:),i=1,loaded_nodes)
 READ(10,*)tol,limit,nlfp
 ALLOCATE(lf(2,nlfp))
 READ(10,*)lf
 nls=FLOOR(lf(1,nlfp)/dtim)
 IF(nstep>nls)nstep=nls
 ALLOCATE(al(nstep))
 CALL load_function(lf,dtim,al)
 kdiag=0
!-----------------------loop the elements to find global arrays sizes-----
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,'x')
   g(1:15:2)=nf(1,num(:))
   g(2:16:2)=nf(2,num(:)) 
   g(17:20)=nf(3,num(1:7:2))
   g_g(:,iel)=g
   g_num(:,iel)=num
   g_coord(:,num)=TRANSPOSE(coord)
   CALL fkdiag(kdiag,g)
 END DO elements_1
 CALL mesh(g_coord,g_num,argv,nlen,12)
 DO i=2,neq
   kdiag(i)=kdiag(i)+kdiag(i-1)
 END DO
 ALLOCATE(kv(kdiag(neq)))
 WRITE(11,'(2(A,I5))')                                                    &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
 loads=zero
 disps=zero
 tensor=zero
 kv=zero
 CALL sample(element,points,weights)                   
!-----------------------global matrix assembly----------------------------
 elements_2: DO iel=1,nels
   kay=zero
   DO i=1,ndim
     kay(i,i)=prop(i,etype(iel))
   END DO
   CALL deemat(dee,prop(3,etype(iel)),prop(4,etype(iel)))
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   g=g_g(:,iel)
   km=zero
   c=zero
   kc=zero
   gauss_points_1: DO i=1,nip
!-----------------------elastic solid contribution------------------------
     CALL shape_der(der,points,i)
     jac=MATMUL(der,coord)
     det=determinant(jac)
     CALL invert(jac)
     deriv=MATMUL(jac,der)
     tensor(1:2,i,iel)=cons
     tensor(4,i,iel)=cons
     tensor(5,i,iel)=zero
     CALL beemat(bee,deriv)
     vol(:)=bee(1,:)+bee(2,:)
     km=km+MATMUL(MATMUL(TRANSPOSE(bee),dee),bee)*det*weights(i)
!-----------------------fluid contribution--------------------------------
     CALL shape_fun(funf,points,i)
     CALL shape_der(derf,points,i)
     derivf=MATMUL(jac,derf)
     kc=kc+MATMUL(MATMUL(TRANSPOSE(derivf),kay),derivf)*det*weights(i)*dtim
     DO k=1,nodf
       volf(:,k)=vol(:)*funf(k)
     END DO
     c=c+volf*det*weights(i)
   END DO gauss_points_1
   store_kc(:,:,iel)=kc
   CALL formke(km,kc,c,ke,theta)
   CALL fsparv(kv,ke,g,kdiag)
 END DO elements_2
 CALL sparin_gauss(kv,kdiag) !---factorise equations----
 pi=ACOS(-one)
 dt=start_dt
 DO i=1,np_types
   phi=prop(5,i)
   snph=SIN(phi*pi/d180)
   e=prop(3,i)
   v=prop(4,i)
   ddt=d4*(one+v)*(one-two*v)/(e*(one-two*v+snph**2))
   IF(ddt<dt)dt=ddt
 END DO
!-----------------------time stepping loop--------------------------------
 oldis=zero
 time=zero
 WRITE(11,'(/A,I5)')" Results at node",nres
 WRITE(11,'(A)')                                                          &
   "    time        load        x-disp      y-disp   porepressure iters"
 WRITE(11,'(5E12.4)')0.0,0.0,0.0,0.0,0.0
 time_steps: DO j=1,nstep
   time=time+dtim
   tot_load=SUM(al(1:j))
   ans=zero
   bdylds=zero
   evpt=zero
   newdis=zero
   elements_3: DO iel=1,nels
     g=g_g(:,iel)
     kc=store_kc(:,:,iel)
     phi0=disps(g(ndof+1:))   ! gather
     phi1=MATMUL(kc,phi0)
     ans(g(ndof+1:))=ans(g(ndof+1:))+phi1   ! scatter
   END DO elements_3
   ans(0)=zero
   iters=0
!-----------------------apply loading increment---------------------------
   DO i=1,loaded_nodes
     ans(nf(1:2,no(i)))=val(i,:)*al(j)
   END DO
!-----------------------plastic iteration loop----------------------------
   its: DO
     iters=iters+1
     loads=ans+bdylds
     CALL spabac_gauss(kv,loads,kdiag)
     WRITE(*,'(A,I6,A,I4)')" time step",j,"  iteration",iters
!-----------------------check plastic convergence-------------------------
     newdis=loads
     newdis(nf(3,:))=zero 
     CALL checon(newdis,oldis,tol,converged)
     IF(iters==1)converged=.FALSE.
     IF(converged.OR.iters==limit)bdylds=zero
!-----------------------go round the Gauss Points ------------------------
     elements_4: DO iel=1,nels
       num=g_num(:,iel)
       coord=TRANSPOSE(g_coord(:,num))
       phi=prop(5,etype(iel))
       coh=prop(6,etype(iel))
       psi=prop(7,etype(iel))
       g=g_g(:,iel)
       eld=loads(g(1:ndof))
       bload=zero
       CALL deemat(dee,prop(3,etype(iel)),prop(4,etype(iel)))     
       gauss_points_2: DO i=1,nip
       CALL shape_der(der,points,i)
         jac=MATMUL(der,coord)
         det=determinant(jac)
         CALL invert(jac)
         deriv=MATMUL(jac,der)
         CALL beemat(bee,deriv)
         eps=MATMUL(bee,eld)
         eps=eps-evpt(:,i,iel)
         sigma=MATMUL(dee,eps)
         stress=sigma+tensor(1:4,i,iel)
         CALL invar(stress,sigm,dsbar,lode_theta)
!-----------------------check whether yield is violated-------------------
         CALL mocouf(phi,coh,sigm,dsbar,lode_theta,f)
         IF(converged.OR.iters==limit)THEN
           devp=stress
         ELSE 
           IF(f>=zero)THEN
             CALL mocouq(psi,dsbar,lode_theta,dq1,dq2,dq3)
             CALL formm(stress,m1,m2,m3)
             flow=f*(m1*dq1+m2*dq2+m3*dq3)
             erate=MATMUL(flow,stress)
             evp=erate*dt
             evpt(:,i,iel)=evpt(:,i,iel)+evp
             devp=MATMUL(dee,evp)
           END IF
         END IF
         IF(f>=zero)THEN
           eload=MATMUL(TRANSPOSE(bee),devp)
           bload=bload+eload*det*weights(i)
         END IF
         IF(converged.OR.iters==limit)THEN
!--------------update the Gauss Point stresses and porepressures----------
           tensor(1:4,i,iel)=stress
           dpore=zero
           CALL shape_fun(funf,points,i)
           DO k=1,nodf
             dpore=dpore+funf(k)*loads(g(k+ndof))
           END DO
           tensor(5,i,iel)=tensor(5,i,iel)+dpore
         END IF
       END DO gauss_points_2
!-----------------------compute the total bodyloads vector ---------------
       bdylds(g(1:ndof))=bdylds(g(1:ndof))+bload
     END DO elements_4
     bdylds(0)=zero
     IF(converged.OR.iters==limit)EXIT
   END DO its
   disps=disps+loads
   IF(j/npri*npri==j.OR.iters==limit)WRITE(11,'(5E12.4,I5)')              &
     time,tot_load,disps(nf(:,nres)),iters
   IF(iters==limit)EXIT
 END DO time_steps
 CALL dismsh(loads,nf,0.05_iwp,g_coord,g_num,argv,nlen,13)
 CALL vecmsh(loads,nf,0.05_iwp,0.1_iwp,g_coord,g_num,argv,nlen,14)

END SUBROUTINE p96
