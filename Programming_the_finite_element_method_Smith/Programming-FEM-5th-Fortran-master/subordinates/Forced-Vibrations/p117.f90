SUBROUTINE p117(input_file,output_file)
!-------------------------------------------------------------------------
! Program 11.7 Forced vibration analysis of an elastic solid in plane
!              strain using rectangular 8-node quadrilaterals. Lumped or
!              consistent mass. Mesh numbered in x- or y-direction.
!              Implicit time integration using the "theta" method.
!              No global matrix assembly. Diagonally preconditioned
!              conjugate gradient solver.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::cg_iters,cg_limit,i,j,iel,k,loaded_nodes,ndim=2,ndof=16,nels,   &
   neq,nip=9,nlen,nn,nod=8,nodof=2,npri,nprops=3,np_types,nr,nres,nst=3,  &
   nstep,nxe,nye
 REAL(iwp)::alpha,area,beta,cg_tol,c1,c2,c3,c4,det,dtim,fk,fm,one=1.0_iwp,&
   theta,time,up,zero=0.0_iwp
 LOGICAL::consistent=.FALSE.,cg_converged
 CHARACTER(LEN=15)::argv,element='quadrilateral'
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),nf(:,:),node(:),  &
   num(:)
 REAL(iwp),ALLOCATABLE::bee(:,:),coord(:,:),d(:),dee(:,:),der(:,:),       &
   deriv(:,:),diag_precon(:),d1x0(:),d1x1(:),d2x0(:),d2x1(:),ecm(:,:),    &
   fun(:),g_coord(:,:),jac(:,:),km(:,:),loads(:),mm(:,:),p(:),points(:,:),&
   prop(:,:),storkm(:,:,:),stormm(:,:,:),u(:),val(:,:),weights(:),x(:),   &
   xnew(:),x0(:),x1(:),x_coords(:),y_coords(:)
!-----------------------input and initialisation--------------------------
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,cg_tol,cg_limit,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ALLOCATE(nf(nodof,nn),points(nip,ndim),g(ndof),g_coord(ndim,nn),         &
   dee(nst,nst),coord(nod,ndim),jac(ndim,ndim),weights(nip),der(ndim,nod),&
   deriv(ndim,nod),bee(nst,ndof),km(ndof,ndof),num(nod),g_num(nod,nels),  &
   g_g(ndof,nels),mm(ndof,ndof),ecm(ndof,ndof),fun(nod),etype(nels),      &
   prop(nprops,np_types),x_coords(nxe+1),y_coords(nye+1),                 &
   storkm(ndof,ndof,nels),stormm(ndof,ndof,nels))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)x_coords,y_coords,dtim,nstep,theta,npri,nres,fm,fk
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)
 ALLOCATE(x0(0:neq),d1x0(0:neq),x1(0:neq),d2x0(0:neq),loads(0:neq),       &
   d1x1(0:neq),d2x1(0:neq),d(0:neq),p(0:neq),x(0:neq),xnew(0:neq),        &
   diag_precon(0:neq),u(0:neq))
 READ(10,*)loaded_nodes
 ALLOCATE(node(loaded_nodes),val(loaded_nodes,ndim))
 READ(10,*)(node(i),val(i,:),i=1,loaded_nodes)
!---------------loop the elements to set up element data------------------
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,'y')
   CALL num_to_g(num,nf,g)
   g_num(:,iel)=num
   g_coord(:,num)=TRANSPOSE(coord)
   g_g(:,iel)=g
 END DO elements_1
 CALL sample(element,points,weights)
 diag_precon=zero
 WRITE(11,'(A,I5,A)')" There are",neq," equations"
 c1=(one-theta)*dtim
 c2=fk-c1
 c3=fm+one/(theta*dtim)
 c4=fk+theta*dtim
 CALL sample(element,points,weights)
 diag_precon=zero
!----element stiffness and mass integration, storage and preconditioner---
 elements_2: DO iel=1,nels
   CALL deemat(dee,prop(1,etype(iel)),prop(2,etype(iel)))
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   g=g_g(:,iel)
   km=zero
   mm=zero
   area=zero
   gauss_pts_1: DO i=1,nip
     CALL shape_der(der,points,i)
     CALL shape_fun(fun,points,i)
     jac=MATMUL(der,coord)
     det=determinant(jac)
     CALL invert(jac)
     deriv=MATMUL(jac,der)
     CALL beemat(bee,deriv)
     km=km+MATMUL(MATMUL(TRANSPOSE(bee),dee),bee)*det*weights(i)
     area=area+det*weights(i)
     IF(consistent)THEN
       CALL ecmat(ecm,fun,ndof,nodof)
       ecm=ecm*det*weights(i)
       mm=mm+ecm
     END IF
   END DO gauss_pts_1
   IF(.NOT.consistent)CALL elmat(area,prop(3,etype(iel)),mm)
   storkm(:,:,iel)=km
   stormm(:,:,iel)=mm
   DO k=1,ndof
     diag_precon(g(k))=diag_precon(g(k))+mm(k,k)*c3+km(k,k)*c4
   END DO
 END DO elements_2 
 diag_precon(1:)=one/diag_precon(1:)
 diag_precon(0)=zero
!-----------------------time stepping loop--------------------------------
 x0=zero
 d1x0=zero
 d2x0=zero
 time=zero
 WRITE(11,'(/A,I5)')" Result at node",nres
 WRITE(11,'(A)')                                                          &
   "    time        load        x-disp      y-disp     cg iters"
 WRITE(11,'(4E12.4)')time,load(time),x0(nf(:,nres))
 timesteps: DO j=1,nstep
   time=time+dtim
   loads=zero
   u=zero
   elements_3: DO iel=1,nels  
     g=g_g(:,iel)
     km=storkm(:,:,iel)
     mm=stormm(:,:,iel)
     u(g)=u(g)+MATMUL(km*c2+mm*c3,x0(g))+MATMUL(mm/theta,d1x0(g))
   END DO elements_3
   u(0)=zero
   DO i=1,loaded_nodes
     loads(nf(:,node(i)))=                                                &
       val(i,:)*(theta*dtim*load(time)+c1*load(time-dtim))
   END DO
   loads=u+loads
   d=diag_precon*loads
   p=d
   x=zero
   cg_iters=0
!-----------------------pcg equation solution-----------------------------
   pcg: DO
     cg_iters=cg_iters+1
     u=zero
     elements_4: DO iel=1,nels
       g=g_g(:,iel)
       km=storkm(:,:,iel)
       mm=stormm(:,:,iel)
       u(g)=u(g)+MATMUL(mm*c3+km*c4,p(g))
     END DO elements_4
     u(0)=zero
     up=DOT_PRODUCT(loads,d)
     alpha=up/DOT_PRODUCT(p,u)
     xnew=x+p*alpha
     loads=loads-u*alpha
     d=diag_precon*loads
     beta=DOT_PRODUCT(loads,d)/up
     p=d+p*beta
     call checon(xnew,x,cg_tol,cg_converged)
     IF(cg_converged.OR.cg_iters==cg_limit)EXIT
   END DO pcg
   x1=xnew
   d1x1=(x1-x0)/(theta*dtim)-d1x0*(one-theta)/theta
   d2x1=(d1x1-d1x0)/(theta*dtim)-d2x0*(one-theta)/theta
   IF(j/npri*npri==j)                                                     &
     WRITE(11,'(4E12.4,I8)')time,load(time),x1(nf(:,nres)),cg_iters
   x0=x1
   d1x0=d1x1
   d2x0=d2x1
 END DO timesteps

CONTAINS
FUNCTION load(t) RESULT(load_result)
!-----------------------Load-time function--------------------------------
 IMPLICIT NONE     
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::t
 REAL(iwp)::load_result
 load_result=COS(0.3_iwp*t)
RETURN
END FUNCTION load
END SUBROUTINE p117
