SUBROUTINE p75(input_file,output_file) 
!-------------------------------------------------------------------------
! SUBROUTINE 7.5 General two- (plane) or three-dimensional analysis of steady
!             seepage. No global conductivity matrix assembly.
!             Diagonally preconditioned conjugate gradient solver.
!-------------------------------------------------------------------------
 USE main; 
 USE geom; 
 IMPLICIT NONE 
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file    
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::cg_iters,cg_limit,fixed_freedoms,i,iel,k,loaded_nodes,nci,ndim, &
   nels,neq,nip,nlen,nod,nn,np_types
 REAL(iwp)::alpha,beta,cg_tol,det,one=1.0_iwp,penalty=1.0e20_iwp,up,      &
   zero=0.0_iwp
 CHARACTER(LEN=15)::argv,element; 
 LOGICAL::cg_converged
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g_num(:,:),node(:),num(:)
 REAL(iwp),ALLOCATABLE::coord(:,:),d(:),der(:,:),deriv(:,:),              &
   diag_precon(:),disps(:),g_coord(:,:),jac(:,:),kay(:,:),kp(:,:),        &
   loads(:),p(:),points(:,:),prop(:,:),store(:),storkp(:,:,:),u(:),       &
   value(:),weights(:),x(:),xnew(:)
!-----------------------input and initialisation--------------------------
 
 OPEN(10,FILE=input_file); 
 OPEN(11,FILE=output_file)
 READ(10,*)element,nod,nels,nn,nip,ndim,cg_tol,cg_limit,np_types; 
 neq=nn
 WRITE(11,'(A,I5,A)')" There are",neq," equations"
 ALLOCATE(points(nip,ndim),g_coord(ndim,nn),coord(nod,ndim),etype(nels),  &
   jac(ndim,ndim),weights(nip),num(nod),g_num(nod,nels),der(ndim,nod),    &
   deriv(ndim,nod),kp(nod,nod),kay(ndim,ndim),prop(ndim,np_types),        &
   p(0:neq),loads(0:neq),x(0:neq),xnew(0:neq),u(0:neq),diag_precon(0:neq),&
   d(0:neq),disps(0:neq),storkp(nod,nod,nels))
 READ(10,*)prop; 
 etype=1; 
 IF(np_types>1)READ(10,*)etype; 
 READ(10,*)g_coord 
 READ(10,*)g_num; 
 IF(ndim==2)CALL mesh(g_coord,g_num,argv,nlen,12)
 diag_precon=zero; 
 CALL sample(element,points,weights)   
!----------element conductivity integration, storage and preconditioner--- 
 elements_1: DO iel=1,nels
   kay=zero; 
   DO i=1,ndim; 
     kay(i,i)=prop(i,etype(iel)); 
   END DO
   num=g_num(:,iel); 
   coord=TRANSPOSE(g_coord(:,num)); 
   kp=zero
   gauss_pts_1: DO i=1,nip
     CALL shape_der(der,points,i); 
     jac=MATMUL(der,coord)
     det=determinant(jac); 
     CALL invert(jac); 
     deriv=MATMUL(jac,der)
     kp=kp+MATMUL(MATMUL(TRANSPOSE(deriv),kay),deriv)*det*weights(i)
   END DO gauss_pts_1; 
   storkp(:,:,iel)=kp
   DO k=1,nod; 
     diag_precon(num(k))=diag_precon(num(k))+kp(k,k); 
   END DO
 END DO elements_1
!-----------------------invert the preconditioner and get starting loads--
 loads=zero; 
 READ(10,*)loaded_nodes,(k,loads(k),i=1,loaded_nodes)
 READ(10,*)fixed_freedoms
 IF(fixed_freedoms/=0)THEN
   ALLOCATE(node(fixed_freedoms),value(fixed_freedoms),                   &
     store(fixed_freedoms))
   READ(10,*)(node(i),value(i),i=1,fixed_freedoms)
   diag_precon(node)=diag_precon(node)+penalty
   loads(node)=diag_precon(node)*value; 
   store=diag_precon(node)
 END IF
 diag_precon(1:)=one/diag_precon(1:); 
 diag_precon(0)=zero 
 d=diag_precon*loads; 
 p=d; 
 x=zero; 
 cg_iters=0
 !-----------------------pcg equation solution-----------------------------
 pcg: DO
   cg_iters=cg_iters+1; 
   u=zero 
   elements_2: DO iel=1,nels
     num=g_num(:,iel); 
     kp=storkp(:,:,iel); 
     u(num)=u(num)+MATMUL(kp,p(num)) 
   END DO elements_2
   IF(fixed_freedoms/=0)u(node)=p(node)*store; 
   up=DOT_PRODUCT(loads,d)
   alpha=up/DOT_PRODUCT(p,u); 
   xnew=x+p*alpha; 
   loads=loads-u*alpha
   d=diag_precon*loads; 
   beta=DOT_PRODUCT(loads,d)/up; 
   p=d+p*beta
   CALL checon(xnew,x,cg_tol,cg_converged)
   IF(cg_converged.OR.cg_iters==cg_limit)EXIT
 END DO pcg
 WRITE(11,'(A,I5)')" Number of cg iterations to convergence was",cg_iters
!-----------------------retrieve nodal net flow rates---------------------
 loads=xnew; 
 disps=zero
 elements_3: DO iel=1,nels
   num=g_num(:,iel); 
   kp=storkp(:,:,iel)
   disps(num)=disps(num)+MATMUL(kp,loads(num)) 
 END DO elements_3; 
 disps(0)=zero
 WRITE(11,'(/A)')"  Node Total Head  Flow rate"
 DO k=1,nn; 
   WRITE(11,'(I5,2E12.4)')k,loads(k),disps(k); 
 END DO
 WRITE(11,'(/A)')"       Inflow      Outflow"
 WRITE(11,'(5X,2E12.4)')                                                  &
   SUM(disps,MASK=disps>zero),SUM(disps,MASK=disps<zero)
 IF(ndim==2.AND.nod==4)THEN
   READ(10,*)nci; 
   CALL contour(loads,g_coord,g_num,nci,argv,nlen,13)
 END IF

END SUBROUTINE p75
