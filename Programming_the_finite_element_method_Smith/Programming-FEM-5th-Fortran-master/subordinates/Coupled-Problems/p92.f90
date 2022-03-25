SUBROUTINE p92(input_file,output_file)
!--------------------------------------------------------------------------
! Program 9.2 Analysis of the plane steady state Navier-Stokes equation
!             using 8-node rectangular quadrilaterals for velocities
!             coupled to 4-node rectangular quadrilaterals for pressures.
!             Mesh numbered in x- or y-direction. Freedoms numbered in the
!             order u-p-v. Element by element solution using BiCGStab(l).
!             with no preconditioning. No global matrix assembly,
!--------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::cg_iters,cg_limit,cg_tot,ell,fixed_freedoms,i,iel,iters,j,k,    &
   limit,ndim=2,nels,neq,nip=4,nlen,nn,nod=8,nodf=4,nodof=3,nr,ntot=20,   &
   nxe,nye
 REAL(iwp)::alpha,beta,cg_tol,det,error,gama,kappa,norm_r,omega,          &
   one=1.0_iwp,penalty=1.e5_iwp,pt5=0.5_iwp,rho,rho1,r0_norm,tol,ubar,    &
   vbar,visc,x0,zero=0.0_iwp
 LOGICAL::converged,cg_converged
 CHARACTER(LEN=15)::argv,element='quadrilateral'
!------------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::g(:),g_g(:,:),g_num(:,:),nf(:,:),no(:),node(:),     &
   num(:),sense(:)
 REAL(iwp),ALLOCATABLE::b(:),coord(:,:),coordf(:,:),c11(:,:),c12(:,:),    &
   c21(:,:),c23(:,:),c32(:,:),der(:,:),derf(:,:),deriv(:,:),derivf(:,:),  &
   diag(:),fun(:),funf(:),gamma(:),gg(:,:),g_coord(:,:),jac(:,:),kay(:,:),&
   ke(:,:),loads(:),nd1(:,:),nd2(:,:),nfd1(:,:),nfd2(:,:),ndf1(:,:),      &
   ndf2(:,:),oldlds(:),points(:,:),r(:,:),rt(:),s(:),store(:),            &
   storke(:,:,:),u(:,:),uvel(:),value(:),vvel(:),weights(:),x_coords(:),  &
   y(:),y1(:),y_coords(:)
!------------------------input and initialisation-------------------------
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,tol,limit,visc,rho,cg_tol,cg_limit,x0,ell,kappa
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ALLOCATE(points(nip,ndim),coord(nod,ndim),derivf(ndim,nodf),             &
   jac(ndim,ndim),kay(ndim,ndim),der(ndim,nod),deriv(ndim,nod),           &
   derf(ndim,nodf),funf(nodf),coordf(nodf,ndim),nd1(nod,nod),nd2(nod,nod),&
   ndf1(nod,nodf),ndf2(nod,nodf),nfd1(nodf,nod),nfd2(nodf,nod),           &
   g_g(ntot,nels),c11(nod,nod),c12(nod,nodf),c21(nodf,nod),g(ntot),       &
   ke(ntot,ntot),fun(nod),x_coords(nxe+1),y_coords(nye+1),nf(nodof,nn),   &
   g_coord(ndim,nn),g_num(nod,nels),num(nod),weights(nip),c32(nod,nodf),  &
   c23(nodf,nod),uvel(nod),vvel(nod),storke(ntot,ntot,nels),s(ell+1),     &
   gg(ell+1,ell+1),gamma(ell+1))
   READ(10,*)x_coords,y_coords
 uvel=zero
 vvel=zero
 kay=zero
 kay(1,1)=visc/rho
 kay(2,2)=visc/rho
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)
 CALL sample(element,points,weights)
!------------------------loop the elements to set up global arrays---------
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,'x')
   g(:8)=nf(1,num(:8))
   g(9:12)=nf(2,num(1:7:2))
   g(13:20)=nf(3,num(:8))
   g_num(:,iel )=num
   g_coord(:,num)=TRANSPOSE(coord)
   g_g(:,iel)=g
 END DO elements_1
 CALL mesh(g_coord,g_num,argv,nlen,12)
 WRITE(11,'(A,I5,A)')" There are",neq," equations"
 ALLOCATE(loads(0:neq),rt(0:neq),r(0:neq,ell+1),u(0:neq,ell+1),b(0:neq),  &
   diag(0:neq),oldlds(0:neq),y(0:neq),y1(0:neq))
 READ(10,*)fixed_freedoms
 ALLOCATE(node(fixed_freedoms),sense(fixed_freedoms),                     &
   value(fixed_freedoms),no(fixed_freedoms),store(fixed_freedoms))
 READ(10,*)(node(i),sense(i),value(i),i=1,fixed_freedoms)
 iters=0
 cg_tot=0
 loads=zero
 oldlds=zero
!------------------------iteration loop------------------------------------
 iterations: DO
   iters=iters+1
   converged=.FALSE.
   ke=zero
   diag=zero
   b=zero
!------------------------element stiffness integration and storage --------
   elements_2: DO iel=1,nels
     num=g_num(:,iel)
     coord=TRANSPOSE(g_coord(:,num))
     g=g_g(:,iel)
     coordf=coord(1:7:2,:)
     uvel=(loads(g(:nod))+oldlds(g(:nod)))*pt5
     DO i=nod+nodf+1,ntot
       vvel(i-nod-nodf)=(loads(g(i))+oldlds(g(i)))*pt5
     END DO
     c11=zero
     c12=zero
     c21=zero
     c23=zero
     c32=zero
     gauss_points_1: DO i=1,nip
!------------------------velocity contribution-----------------------------
       CALL shape_fun(fun,points,i)
       CALL shape_der(der,points,i)
       jac=MATMUL(der,coord)
       det=determinant(jac)
       CALL invert(jac)
       deriv=MATMUL(jac,der)
       ubar=DOT_PRODUCT(fun,uvel)
       vbar=DOT_PRODUCT(fun,vvel)
       IF(iters==1)THEN
         ubar=one
         vbar=zero
       END IF
       CALL cross_product(fun,deriv(1,:),nd1)
       CALL cross_product(fun,deriv(2,:),nd2)
       c11=c11+det*weights(i)*(MATMUL(MATMUL(TRANSPOSE(deriv),kay),deriv)+ &
         nd1*ubar+nd2*vbar)
!------------------------pressure contribution-----------------------------
       CALL shape_fun(funf,points,i)
       CALL shape_der(derf,points,i)
       jac=MATMUL(derf,coordf)
       det=determinant(jac)
       CALL invert(jac)
       derivf=MATMUL(jac,derf)
       CALL cross_product(fun,derivf(1,:),ndf1)
       CALL cross_product(fun,derivf(2,:),ndf2)
       CALL cross_product(funf,deriv(1,:),nfd1)
       CALL cross_product(funf,deriv(2,:),nfd2)
       c12=c12+ndf1*det*weights(i)/rho
       c32=c32+ndf2*det*weights(i)/rho
       c21=c21+nfd1*det*weights(i)
       c23=c23+nfd2*det*weights(i)
     END DO gauss_points_1
     CALL formupv(ke,c11,c12,c21,c23,c32)
     storke(:,:,iel)=ke
     DO k=1,ntot
       diag(g(k))=diag(g(k))+ke(k,k)
     END DO
   END DO elements_2
!------------------------specify pressure and velocity boundary values-----
   DO i=1,fixed_freedoms
     no(i)=nf(sense(i),node(i))
   END DO
   diag(no)=diag(no)+penalty
   b(no)=diag(no)*value
   store=diag(no)
!---solve the simultaneous equations element by element using BiCGStab(l)-
!------------------------initialisation phase------------------------------
   IF(iters==1)loads=x0
   loads(0)=zero
   y=loads
   y1=zero
   elements_3: DO iel=1,nels
     g=g_g(:,iel)
     ke=storke(:,:,iel)
     y1(g)=y1(g)+MATMUL(ke,y(g))
   END DO elements_3
   cg_iters=0
   y1(0)=zero
   y1(no)=y(no)*store
   y=y1
   rt=b-y
   r=zero
   r(:,1)=rt
   u=zero
   gama=one
   omega=one
   k=0
   norm_r=norm(rt)
   r0_norm=norm_r
   error=one
!------------------------BiCGStab(l) iterations----------------------------
   bicg_iterations: DO
     cg_iters=cg_iters+1
     cg_converged=error<cg_tol
     IF(cg_iters==cg_limit.OR.cg_converged)EXIT
     gama=-omega*gama
     y=r(:,1)
     DO j=1,ell
       rho1=DOT_PRODUCT(rt,y)
       beta=rho1/gama
       u(:,:j)=r(:,:j)-beta*u(:,:j)
       y=u(:,j)
       y1=zero
       elements_4: DO iel=1,nels
         g=g_g(:,iel)
         ke=storke(:,:,iel)
         y1(g)=y1(g)+MATMUL(ke,y(g))
       END DO elements_4
       y1(0)=zero
       y1(no)=y(no)*store
       y=y1
       u(:,j+1)=y
       gama=DOT_PRODUCT(rt,y)
       alpha=rho1/gama
       loads=loads+alpha*u(:,1)
       r(:,:j)=r(:,:j)-alpha*u(:,2:j+1)
       y=r(:,j)
       y1=zero
       elements_5: DO iel=1,nels
         g=g_g(:,iel)
         ke=storke(:,:,iel)
         y1(g)=y1(g)+MATMUL(ke,y(g))
       END DO elements_5
       y1(0)=zero
       y1(no)=y(no)*store
       y=y1
       r(:,j+1)=y
     END DO
     gg=MATMUL(TRANSPOSE(r),r)
     CALL form_s(gg,ell,kappa,omega,gamma,s)
     loads=loads-MATMUL(r,s)
     r(:,1)=MATMUL(r,gamma)
     u(:,1)=MATMUL(u,gamma)
     norm_r=norm(r(:,1))
     error=norm_r/r0_norm
     k=k+1
   END DO bicg_iterations
   cg_tot=cg_tot+cg_iters
!------------------------end of BiCGStab(l) process-----------------------
   CALL checon(loads,oldlds,tol,converged)
   IF(converged.OR.iters==limit)EXIT
 END DO iterations
 WRITE(11,'(/A)')"  Node     u-velocity  pressure    v-velocity"
 DO k=1,nn
   WRITE(11,'(I5,A,3E12.4)')k,"    ",loads(nf(:,k))
 END DO
 WRITE(11,'(/A,I3,A/A,F6.2,A)')" Converged in",iters," iterations",       &
   " with an average of", REAL(cg_tot/iters), "  BiCGStab(l) iterations."
 CALL vecmsh(loads,nf(1:3:2,:),0.3_iwp,0.05_iwp,g_coord,g_num,argv,nlen,14)

END SUBROUTINE p92
