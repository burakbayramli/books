SUBROUTINE p91(input_file,output_file)   
!-------------------------------------------------------------------------
! Program 9.1 Analysis of the plane steady state Navier-Stokes equation
!             using 8-node rectangular quadrilaterals for velocities
!             coupled to 4-node rectangular quadrilaterals for pressures.
!             Mesh numbered in x-direction. Freedoms numbered in the
!             order u-p-v.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,iters,k,limit,nband,ndim=2,nels,neq,nip=4, &
   nlen,nn,nod=8,nodf=4,nodof=3,nr,ntot=20,nxe,nye
 REAL(iwp)::det,one=1.0_iwp,penalty=1.e20_iwp,pt5=0.5_iwp,rho,tol,ubar,   &
  vbar,visc,zero=0.0_iwp 
 CHARACTER(LEN=15)::argv,element='quadrilateral'
 LOGICAL::converged
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::g(:),g_g(:,:),g_num(:,:),nf(:,:),no(:),node(:),     &
   num(:),sense(:)
 REAL(iwp),ALLOCATABLE::coord(:,:),coordf(:,:),c11(:,:),c12(:,:),c21(:,:),&
   c23(:,:),c32(:,:),der(:,:),derf(:,:),deriv(:,:),derivf(:,:),fun(:),    &
   funf(:),g_coord(:,:),jac(:,:),kay(:,:),ke(:,:),loads(:),nd1(:,:),      &
   nd2(:,:),ndf1(:,:),ndf2(:,:),nfd1(:,:),nfd2(:,:),oldlds(:),pb(:,:),    &
   points(:,:),uvel(:),value(:),vvel(:),weights(:),work(:,:),x_coords(:), &
   y_coords(:)
!-----------------------input and initialisation--------------------------
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,tol,limit,visc,rho
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ALLOCATE(points(nip,ndim),coord(nod,ndim),derivf(ndim,nodf),uvel(nod),   &
   jac(ndim,ndim),kay(ndim,ndim),der(ndim,nod),deriv(ndim,nod),vvel(nod), &
   derf(ndim,nodf),funf(nodf),coordf(nodf,ndim),g_g(ntot,nels),           &
   c11(nod,nod),c12(nod,nodf),c21(nodf,nod),c23(nodf,nod),g(ntot),        &
   c32(nod,nodf),ke(ntot,ntot),fun(nod),x_coords(nxe+1),y_coords(nye+1),  &
   nf(nodof,nn),g_coord(ndim,nn),g_num(nod,nels),num(nod),weights(nip),   &
   nd1(nod,nod),nd2(nod,nod),ndf1(nod,nodf),ndf2(nod,nodf),nfd1(nodf,nod),&
   nfd2(nodf,nod))
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
 nband=0
!-----------------------loop the elements to find global arrays sizes-----
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,'x')
   g(1:8)=nf(1,num(1:8))
   g(9:12)=nf(2,num(1:7:2))
   g(13:20)=nf(3,num(1:8))
   g_num(:,iel)=num;g_coord(:,num)=TRANSPOSE(coord)
   g_g(:,iel)=g
   IF(nband<bandwidth(g))nband=bandwidth(g)
 END DO elements_1
 CALL mesh(g_coord,g_num,argv,nlen,12)
 WRITE(11,'(2(A,I5))')                                                    &
   " There are",neq," equations and the full bandwidth is",2*(nband+1)-1
 ALLOCATE(pb(neq,2*(nband+1)-1),loads(0:neq),oldlds(0:neq),               &
   work(nband+1,neq))
 loads=zero
 oldlds=zero
 iters=0
 READ(10,*)fixed_freedoms
 ALLOCATE(node(fixed_freedoms),sense(fixed_freedoms),                     &
   value(fixed_freedoms),no(fixed_freedoms))
 READ(10,*)(node(i),sense(i),value(i),i=1,fixed_freedoms)
!-----------------------iteration loop------------------------------------
 iterations: DO
   iters=iters+1
   converged=.FALSE.
   pb=zero
   work=zero
   ke=zero
!-----------------------global matrix assembly----------------------------
   elements_2: DO iel=1,nels
     num=g_num(:,iel)
     coord=TRANSPOSE(g_coord(:,num))
     g=g_g(:,iel)
     coordf=coord(1:7:2,:)
     uvel=(loads(g(1:nod))+oldlds(g(1:nod)))*pt5
     DO i=nod+nodf+1,ntot 
       vvel(i-nod-nodf)=(loads(g(i))+oldlds(g(i)))*pt5
     END DO
     c11=zero
     c12=zero
     c21=zero
     c23=zero
     c32=zero
     gauss_points_1: DO i=1,nip
!-----------------------velocity contribution-----------------------------
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
       c11=c11+det*weights(i)*(MATMUL(MATMUL(TRANSPOSE(deriv),kay),deriv)+&
         nd1*ubar+nd2*vbar)
!-----------------------pressure contribution-----------------------------
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
     CALL formtb(pb,ke,g)
   END DO elements_2
   loads=zero
!-----------------------specify pressure and velocity boundary values-----
   DO i=1,fixed_freedoms
     no(i)=nf(sense(i),node(i))
   END DO
   pb(no,nband+1)=pb(no,nband+1)+penalty
   loads(no)=pb(no,nband+1)*value
!-----------------------equation solution---------------------------------
   CALL gauss_band(pb,work)
   CALL solve_band(pb,work,loads)
   loads(0)=zero
   CALL checon(loads,oldlds,tol,converged)   
   IF(converged.OR.iters==limit)EXIT
 END DO iterations
 WRITE(11,'(/A)')"  Node     u-velocity  pressure    v-velocity"
 DO k=1,nn
   WRITE(11,'(I5,A,3E12.4)')k,"    ",loads(nf(:,k))
 END DO
 WRITE(11,'(/A,I3,A)')" Converged in",iters," iterations."
 CALL vecmsh(loads,nf(1:3:2,:),0.3_iwp,0.05_iwp,g_coord,g_num,argv,nlen,14)
 
END SUBROUTINE p91 
