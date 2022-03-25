SUBROUTINE p85(input_file,output_file)
!-------------------------------------------------------------------------
! Program 8.5 Plane or axisymmetric consolidation analysis using 4-node
!             rectangular quadrilaterals. Mesh numbered in x(r)- or y(z)-
!             direction. Implicit time integration using the "theta"
!             method. No global matrix assembly. Diagonal
!             preconditioner conjugate gradient solver
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::cg_iters,cg_limit,fixed_freedoms,i,iel,j,k,nci,ndim=2,nels,neq, &
   nip=4,nlen,nn,nod=4,npri,np_types,nres,nstep,ntime,nxe,nye
 REAL(iwp)::alpha,beta,cg_tol,det,dtim,one=1.0_iwp,penalty=1.0e20_iwp,    &
   theta,time,up,zero=0.0_iwp
 LOGICAL::cg_converged
 CHARACTER(LEN=15)::argv,dir,element='quadrilateral',type_2d
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g_num(:,:),node(:),num(:)
 REAL(iwp),ALLOCATABLE::coord(:,:),d(:),der(:,:),deriv(:,:),              &
   diag_precon(:),fun(:),gc(:),g_coord(:,:),jac(:,:),kay(:,:),kc(:,:),    &
   loads(:),ntn(:,:),p(:),mm(:,:),points(:,:),prop(:,:),r(:),store(:),    &
   storka(:,:,:),storkb(:,:,:),u(:),value(:),weights(:),x(:),xnew(:),     &
   x_coords(:),y_coords(:)
!-----------------------input and initialisation--------------------------
 
 OPEN(10,FILE=input_file) 
 OPEN(11,FILE=output_file)
 READ(10,*)type_2d,dir,nxe,nye,cg_tol,cg_limit,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 neq=nn
 WRITE(11,'(a,i5,a)')" There are",neq," equations"
 ALLOCATE(points(nip,ndim),weights(nip),kay(ndim,ndim),coord(nod,ndim),   &
   fun(nod),jac(ndim,ndim),g_coord(ndim,nn),der(ndim,nod),deriv(ndim,nod),&
   mm(nod,nod),g_num(nod,nels),kc(nod,nod),ntn(nod,nod),num(nod),         &
   storka(nod,nod,nels),storkb(nod,nod,nels),etype(nels),x_coords(nxe+1), &
   y_coords(nye+1),prop(ndim,np_types),loads(0:neq),diag_precon(0:neq),   &
   u(0:neq),d(0:neq),p(0:neq),x(0:neq),r(0:neq),xnew(0:neq),gc(ndim))
 READ(10,*)prop
 etype=1
 IF(np_types>1)read(10,*)etype
 READ(10,*)x_coords,y_coords
 READ(10,*)dtim,nstep,theta,npri,nres,ntime
!---------------loop the elements to set up element data------------------
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,dir)
   g_num(:,iel)=num
   g_coord(:,num)=TRANSPOSE(coord)
 END DO elements_1
 CALL mesh(g_coord,g_num,argv,nlen,12)
 CALL sample(element,points,weights)
 diag_precon=zero
 gc=one
!----------element matrix integration, storage and preconditioner------ 
 elements_2: DO iel=1,nels
   kay=zero
   DO i=1,ndim
     kay(i,i)=prop(i,etype(iel))
   END DO
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   kc=zero
   mm=zero
   gauss_pts: DO i=1,nip
     CALL shape_der(der,points,i)
     CALL shape_fun(fun,points,i)
     jac=MATMUL(der,coord)
     det=determinant(jac)
     CALL invert(jac)
     deriv=MATMUL(jac,der)
     IF(type_2d=='axisymmetric')gc=MATMUL(fun,coord)
     kc=kc+MATMUL(MATMUL(TRANSPOSE(deriv),kay),deriv)*det*weights(i)*gc(1)
     CALL cross_product(fun,fun,ntn)
     mm=mm+ntn*det*weights(i)*gc(1)
   END DO gauss_pts
   storka(:,:,iel)=mm+kc*theta*dtim
   storkb(:,:,iel)=mm-kc*(one-theta)*dtim
   DO k=1,nod 
     diag_precon(num(k))=diag_precon(num(k))+storka(k,k,iel)
   END DO
 END DO elements_2
!-----------------------specify initial and boundary values---------------
 READ(10,*)loads(1:)
 loads(0)=zero
 READ(10,*)fixed_freedoms
 IF(fixed_freedoms/=0)THEN
   ALLOCATE(node(fixed_freedoms),value(fixed_freedoms),                   &
            store(fixed_freedoms))
   READ(10,*)(node(i),value(i),i=1,fixed_freedoms)
   diag_precon(node)=diag_precon(node)+penalty
   store=diag_precon(node)
 END IF
 diag_precon(1:)=one/diag_precon(1:)
 diag_precon(0)=zero
!-----------------------time stepping loop--------------------------------
 WRITE(11,'(/a,i3,a)')"    Time      Pressure (node",nres,")  cg iters"
 WRITE(11,'(2e12.4)')0.0,loads(nres)
 timesteps: DO j=1,nstep
   time=j*dtim
   u=zero
   elements_3 : DO iel=1,nels    
     num=g_num(:,iel)
     kc=storkb(:,:,iel) 
     u(num)=u(num)+MATMUL(kc,loads(num))  
   END DO elements_3
   u(0)=zero
   r=u
   IF(fixed_freedoms/=0)r(node)=store*value
   d=diag_precon*r
   p=d
   x=zero
   cg_iters=0
!-----------------------pcg equation solution-----------------------------
   pcg: DO
     cg_iters=cg_iters+1
     u=zero
      elements_4: DO iel=1,nels
        num=g_num(:,iel)
        kc=storka(:,:,iel)
        u(num)=u(num)+MATMUL(kc,p(num))
      END DO elements_4
      IF(fixed_freedoms/=0)u(node)=p(node)*store
      up=DOT_PRODUCT(r,d)
      alpha=up/DOT_PRODUCT(p,u)
      xnew=x+p*alpha
      r=r-u*alpha
      d=diag_precon*r
      beta=DOT_PRODUCT(r,d)/up
      p=d+p*beta
      CALL checon(xnew,x,cg_tol,cg_converged)
      IF(cg_converged.OR.cg_iters==cg_limit)EXIT
   END DO pcg
   loads=xnew
   IF(nod==4.AND.j==ntime)THEN
     READ(10,*)nci
     CALL contour(loads,g_coord,g_num,nci,argv,nlen,13)
   END IF
   IF(j/npri*npri==j)                                                      &
     WRITE(11,'(2e12.4,7x,i5)')time,loads(nres),cg_iters
 END DO timesteps

END SUBROUTINE p85
