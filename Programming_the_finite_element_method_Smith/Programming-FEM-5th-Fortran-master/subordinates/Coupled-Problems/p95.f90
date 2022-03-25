SUBROUTINE p95(input_file,output_file)
!-------------------------------------------------------------------------
! Program 9.5 Plane strain consolidation analysis of a Biot elastic solid 
!             using 8-node rectangular quadrilaterals for displacements 
!             coupled to 4-node rectangular quadrilaterals for pressures. 
!             Freedoms numbered in order u-v-uw. Incremental load version.
!             No global stiffness matrix assembly.
!             Diagonally preconditioned conjugate gradient solver.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::cg_iters,cg_limit,i,iel,j,k,loaded_nodes,ndim=2,ndof=16,nels,   &
   neq,nip=4,nlen,nlfp,nls,nn,nod=8,nodf=4,nodof=3,npri,nprops=4,np_types,&
   nr,nres,nst=3,nstep,ntot=20,nxe,nye
 LOGICAL::cg_converged   
 REAL(iwp)::alpha,beta,cg_tol,det,dtim,one=1.0_iwp,theta,time,tot_load,up,&
   zero=0.0_iwp
 CHARACTER(LEN=15)::argv,element='quadrilateral'
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),nf(:,:),no(:),    &
   num(:)
 REAL(iwp),ALLOCATABLE::al(:),ans(:),bee(:,:),c(:,:),coord(:,:),d(:),     &
   dee(:,:),der(:,:),derf(:,:),deriv(:,:),derivf(:,:),diag_precon(:),     &
   eld(:),fun(:),funf(:),gc(:),g_coord(:,:),jac(:,:),kay(:,:),ke(:,:),    &
   km(:,:),kc(:,:),lf(:,:),loads(:),p(:),points(:,:),prop(:,:),sigma(:),  &
   storke(:,:,:),u(:),val(:,:),vol(:),volf(:,:),store_kc(:,:,:),          &
   weights(:),x(:),xnew(:),x_coords(:),y_coords(:),phi0(:),phi1(:)
!-----------------------input and initialisation--------------------------
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,cg_tol,cg_limit,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ALLOCATE(dee(nst,nst),points(nip,ndim),coord(nod,ndim),derivf(ndim,nodf),&
   jac(ndim,ndim),kay(ndim,ndim),der(ndim,nod),deriv(ndim,nod),           &
   derf(ndim,nodf),funf(nodf),bee(nst,ndof),km(ndof,ndof),kc(nodf,nodf),  &
   g_g(ntot,nels),ke(ntot,ntot),c(ndof,nodf),fun(nod),x_coords(nxe+1),    &
   store_kc(nodf,nodf,nels),y_coords(nye+1),vol(ndof),nf(nodof,nn),       &
   g(ntot),volf(ndof,nodf),g_coord(ndim,nn),g_num(nod,nels),num(nod),     &
   weights(nip),storke(ntot,ntot,nels),etype(nels),phi0(nodf),phi1(nodf), &
   prop(nprops,np_types),gc(ndim),sigma(nst),eld(ndof))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)x_coords,y_coords
 READ(10,*)dtim,nstep,theta,npri,nres
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)
 ALLOCATE(loads(0:neq),ans(0:neq),p(0:neq),x(0:neq),xnew(0:neq),u(0:neq), &
   diag_precon(0:neq),d(0:neq))
 READ(10,*)loaded_nodes
 ALLOCATE(no(loaded_nodes),val(loaded_nodes,ndim))
 READ(10,*)(no(i),val(i,:),i=1,loaded_nodes)
 READ(10,*)nlfp
 ALLOCATE(lf(2,nlfp))
 READ(10,*)lf 
 nls=FLOOR(lf(1,nlfp)/dtim)
 IF(nstep>nls)nstep=nls
 ALLOCATE(al(nstep))
 CALL load_function(lf,dtim,al)
!-----------------------loop the elements to set up element data----------
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,'x')
   g(1:15:2)=nf(1,num(:))
   g(2:16:2)=nf(2,num(:))
   g(17:20)=nf(3,num(1:7:2))
   g_g(:,iel)=g
   g_num(:,iel)=num
   g_coord(:,num)=TRANSPOSE(coord)
 END DO elements_1
 CALL mesh(g_coord,g_num,argv,nlen,12)
 WRITE(11,'(A,I5,A)')" There are",neq," equations"
 loads=zero
 p=zero
 xnew=zero
 diag_precon=zero
 CALL sample(element,points,weights)
!----------element matrix integration, storage and preconditioner--------- 
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
     CALL beemat(bee,deriv)
     vol(:)=bee(1,:)+bee(2,:)
     km=km+MATMUL(MATMUL(TRANSPOSE(bee),dee),bee)*det*weights(i)
!-----------------------fluid contribution--------------------------------
     CALL shape_fun(funf,points,i)
     CALL shape_der(derf,points,i)
     derivf=MATMUL(jac,derf)
     kc=kc+MATMUL(MATMUL(TRANSPOSE(derivf),kay),derivf)*det*weights(i)*dtim
     CALL cross_product(vol,funf,volf)
     c=c+volf*det*weights(i)
   END DO gauss_points_1
   store_kc(:,:,iel)=kc
   CALL formke(km,kc,c,ke,theta)
   storke(:,:,iel)=ke
   DO k=1,ndof
     diag_precon(g(k))=diag_precon(g(k))+theta*km(k,k)
   END DO
   DO k=1,nodf
     diag_precon(g(ndof+k))=diag_precon(g(ndof+k))-theta*theta*kc(k,k)
   END DO
 END DO elements_2
 diag_precon(1:)=one/diag_precon(1:)
 diag_precon(0)=zero
!-----------------------time stepping loop--------------------------------
 WRITE(11,'(/A,I5)')" Results at node",nres
 WRITE(11,'(4X,A)')                                                       &
   "time        load        x-disp      y-disp   porepressure  cg iters"
 WRITE(11,'(5E12.4)')0.0,0.0,loads(nf(:,nres))
 time_steps: DO j=1,nstep
   tot_load=SUM(al(1:j))
   time=j*dtim
   ans=zero
   elements_3: DO iel=1,nels
     g=g_g(:,iel)
     kc=store_kc(:,:,iel)
     phi0=loads(g(ndof+1:))   ! gather
     phi1=MATMUL(kc,phi0)
     ans(g(ndof+1:))=ans(g(ndof+1:))+phi1   
   END DO elements_3
   ans(0)=zero
   DO i=1,loaded_nodes
     ans(nf(1:2,no(i)))=val(i,:)*al(j)
   END DO
   d=diag_precon*ans
   p=d
   x=zero
   cg_iters=0
!-----------------------pcg equation solution-----------------------------
   pcg: DO
     cg_iters=cg_iters+1
     u=zero
     elements_4: DO iel=1,nels
       g=g_g(:,iel)
       ke=storke(:,:,iel)
       u(g)=u(g)+MATMUL(ke,p(g))
     END DO elements_4
     up=DOT_PRODUCT(ans,d)
     alpha=up/DOT_PRODUCT(p,u)
     xnew=x+p*alpha
     ans=ans-u*alpha
     d=diag_precon*ans
     beta=DOT_PRODUCT(ans,d)/up
     p=d+p*beta
     CALL checon(xnew,x,cg_tol,cg_converged)
     IF(cg_converged.OR.cg_iters==cg_limit)EXIT
   END DO pcg
   ans=xnew
   ans(0)=zero
   loads=loads+ans
   loads(0)=zero
   IF(j/npri*npri==j)WRITE(11,'(5E12.4,I7)')                              &
     time,tot_load,loads(nf(:,nres)),cg_iters
!-----------------------recover stresses at nip integrating points--------
!   nip=1DEALLOCATE(points,weights)
!   ALLOCATE(points(nip,ndim),weights(nip)) 
!   CALL sample(element,points,weights)
!   WRITE(11,'(A,I2,A)')" The integration point (nip=",nip,") stresses are:"
!   WRITE(11,'(A,A)')" Element x-coord     y-coord",                       &
!                    "     sig_x       sig_y       tau_xy" 
!   elements_5: DO iel=1,nelsnum=g_num(:,iel)
!     coord=TRANSPOSE(g_coord(:,num))g=g_g(:,iel)eld=loads(g(:ndof))
!     CALL deemat(dee,prop(3,etype(iel)),prop(4,etype(iel)))
!     gauss_pts_2: DO i=1,nip
!       CALL shape_fun(fun,points,i)CALL shape_der(der,points,i)
!       gc=MATMUL(fun,coord)jac=MATMUL(der,coord)CALL invert(jac)
!       deriv=MATMUL(jac,der)CALL beemat(bee,deriv)
!       sigma=MATMUL(dee,MATMUL(bee,eld))
!       IF(j/npri*npri==j)WRITE(11,'(I5,6E12.4)')iel,gc,sigma
!     END DO gauss_pts_2
!   END DO elements_5
 END DO time_steps
 CALL dismsh(loads,nf,0.05_iwp,g_coord,g_num,argv,nlen,13)
 CALL vecmsh(loads,nf,0.05_iwp,0.1_iwp,g_coord,g_num,argv,nlen,14)

END SUBROUTINE p95
