SUBROUTINE p57(input_file,output_file)
!-------------------------------------------------------------------------
! Program 5.7 Three-dimensional strain of an elastic solid using
!             8-, 14- or 20-node brick hexahedra. Mesh numbered in x-z
!             planes then in the y-direction. No global stiffness matrix.
!             Diagonally preconditioned conjugate gradient solver.
!             Optimised maths library, Abaqus UMAT version.
!-------------------------------------------------------------------------
 USE main 
 USE geom 
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER,PARAMETER::npri=1,nstep=1
 INTEGER::cg_iters,cg_limit,fixed_freedoms,i,iel,k,loaded_nodes,ndim=3,  &
   ndof,nels,neq,nip,nn,nprops=2,np_types,nod,nodof=3,nr,nst=6,nxe,nye,  &
   nze,nlen,step,idamax
 REAL(iwp)::alpha,beta,big,cg_tol,det,one=1.0_iwp,penalty=1.0e20_iwp,up, &
   zero=0.0_iwp,dtim=0.0_iwp,ddot,maxload,maxdiff
 CHARACTER(LEN=15)::element='hexahedron',argv
 LOGICAL::cg_converged,solid=.TRUE.
!-----------------------variables required by UMAT------------------------
!ntens==nst 
!noel==iel 
!nprops==nprops
 INTEGER,PARAMETER::ndi=3 
 INTEGER::nshr,nstatv,npt,layer,kspt,kstep,kinc
 REAL(iwp)::sse,spd,scd,rpl,dtime,temp,dtemp,pnewdt,celent
 CHARACTER(LEN=80)::cmname
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),nf(:,:),no(:),   &
   node(:),num(:),sense(:)
 REAL(iwp),ALLOCATABLE::bee(:,:),coord(:,:),d(:),dee(:,:),der(:,:),gc(:),&
   deriv(:,:),diag_precon(:),eld(:),fun(:),g_coord(:,:),g_pmul(:,:),     &
   g_utemp(:,:),jac(:,:),km(:,:),loads(:),p(:),points(:,:),prop(:,:),    &
   sigma(:),store(:),storkm(:,:,:),u(:),value(:),weights(:),x(:),xnew(:),&
   x_coords(:),y_coords(:),z_coords(:),oldlds(:),timest(:)
!-----------------------dynamic arrays required by UMAT-------------------
! stress(ntens)==sigma(nst) 
! ddsdde(ntens,ntens)==dee(nst,nst)
! dstran(ntens)==eld(nst) 
! coords()==points()
 REAL(iwp),ALLOCATABLE::statev(:),ddsddt(:),drplde(:),drpldt(:),stran(:),&
   time(:),predef(:),dpred(:),props(:),drot(:,:),dfgrd0(:,:),dfgrd1(:,:)
!-----------------------input and initialisation--------------------------
 ALLOCATE(timest(2)) 
 timest=zero 
 timest(1)=elap_time()
 !CALL getname(argv,nlen)
 OPEN(10,FILE=input_file) 
 OPEN(11,FILE=output_file)
 READ(10,*)nod,nxe,nye,nze,nip,cg_tol,cg_limit,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye,nze)
 ndof=nod*nodof
 ALLOCATE(nf(nodof,nn),points(nip,ndim),dee(nst,nst),coord(nod,ndim),     &
   jac(ndim,ndim),der(ndim,nod),deriv(ndim,nod),fun(nod),gc(ndim),        &
   bee(nst,ndof),km(ndof,ndof),eld(ndof),sigma(nst),g_coord(ndim,nn),     &
   g_num(nod,nels),weights(nip),num(nod),g_g(ndof,nels),x_coords(nxe+1),  &
   g(ndof),y_coords(nye+1),z_coords(nze+1),etype(nels),g_pmul(ndof,nels), &
   prop(nprops,np_types),storkm(ndof,ndof,nels),g_utemp(ndof,nels))
 ALLOCATE(props(nprops)) ! Abaqus UMAT
 READ(10,*)prop 
 etype=1 
 IF(np_types>1)READ(10,*)etype
 READ(10,*)x_coords,y_coords,z_coords
 nf=1 
 READ(10,*)nr,(k,nf(:,k),i=1,nr) 
 CALL formnf(nf) 
 neq=MAXVAL(nf)
 ALLOCATE(p(0:neq),loads(0:neq),x(0:neq),xnew(0:neq),u(0:neq),            &
   diag_precon(0:neq),d(0:neq),oldlds(0:neq))
 diag_precon=zero 
 CALL sample(element,points,weights)
 timest(2)=elap_time()
!----------element stiffness integration, storage and preconditioner------
 elements_1: DO iel=1,nels
   CALL hexahedron_xz(iel,x_coords,y_coords,z_coords,coord,num)
   CALL num_to_g(num,nf,g) 
   g_num(:,iel)=num
   g_coord(:,num)=TRANSPOSE(coord) 
   g_g(:,iel)=g
   props(1)=prop(1,etype(iel)) 
   props(2)=prop(2,etype(iel)) 
   dee=zero
   CALL umat(sigma,statev,dee,sse,spd,scd,rpl,ddsddt,drplde,              &
     drpldt,stran,eld,time,dtime,temp,dtemp,predef,dpred,cmname,ndi,nshr, &
     nst,nstatv,props,nprops,points,drot,pnewdt,celent,dfgrd0,dfgrd1,iel, &
     npt,layer,kspt,kstep,kinc)
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   g=g_g(:,iel)
   km=zero
   gauss_pts_1: DO i=1,nip
     CALL shape_der(der,points,i) 
     jac=MATMUL(der,coord)
     det=determinant(jac) 
     CALL invert(jac)
     deriv=MATMUL(jac,der) 
     CALL beemat(bee,deriv)
     km=km+MATMUL(matmul(transpose(bee),dee),bee)*det*weights(i)
   END DO gauss_pts_1 
   storkm(:,:,iel)=km
   DO k=1,ndof 
     diag_precon(g(k))=diag_precon(g(k))+km(k,k) 
   END DO
 END DO elements_1
!-----------------------invert the preconditioner and get starting loads--
 loads=zero 
 READ(10,*)loaded_nodes,(k,loads(nf(:,k)),i=1,loaded_nodes)
 oldlds = loads 
 READ(10,*)fixed_freedoms
 IF(fixed_freedoms/=0)THEN
   ALLOCATE(node(fixed_freedoms),sense(fixed_freedoms),                   &
     value(fixed_freedoms),no(fixed_freedoms),store(fixed_freedoms))
   READ(10,*)(node(i),sense(i),value(i),i=1,fixed_freedoms)
   DO  i=1,fixed_freedoms 
     no(i)=nf(sense(i),node(i)) 
   END DO
   diag_precon(no)=diag_precon(no)+penalty
   loads(no)=diag_precon(no)*value
   store=diag_precon(no)
 END IF
 CALL mesh_ensi(argv,nlen,g_coord,g_num,element,etype,nf,oldlds(1:),      &
                nstep,npri,dtim,solid)
 diag_precon(1:)=one/diag_precon(1:) 
 diag_precon(0)=zero
 d=diag_precon*loads 
 p=d 
 x=zero 
 cg_iters=0
!-----------------------pcg equation solution-----------------------------
 pcg: DO
   cg_iters=cg_iters+1 
   u=zero 
   g_utemp=zero
   elements_2: DO iel=1,nels 
     g_pmul(:,iel)=p(g_g(:,iel))
   END DO elements_2
   CALL dsymm('L','l',ndof,nels,one,km,ndof,g_pmul,ndof,one,g_utemp,ndof)
   elements_2a: DO iel=1,nels 
     u(g_g(:,iel))=u(g_g(:,iel))+g_utemp(:,iel)
   END DO elements_2a
   IF(fixed_freedoms/=0)u(no)=p(no)*store 
   up=DDOT(neq,loads,1,d,1)
   alpha=up/DDOT(neq,p,1,u,1) 
   CALL daxpy(neq,alpha,p,1,xnew,1)
   alpha=-alpha 
   CALL daxpy(neq,alpha,u,1,loads,1) 
   d=diag_precon*loads
   beta=DDOT(neq,loads,1,d,1)/up 
   p=d+p*beta
   CALL checon(xnew,x,cg_tol,cg_converged)
   IF(cg_converged.OR.cg_iters==cg_limit)EXIT
 END DO pcg
 WRITE(11,'(/A)')"       Node   x-disp      y-disp      z-disp"
 DO k=1,nn 
   WRITE(11,'(I10,3E12.4)')k,xnew(nf(:,k)) 
 END DO
 CALL dismsh_ensi(argv,nlen,nstep,nf,xnew(1:))
!-----------------------recover stresses at nip integrating point---------
 nip=1 
 DEALLOCATE(points,weights) 
 ALLOCATE(points(nip,ndim),weights(nip))
 WRITE(11,'(/A,I2,A)')" The integration point (nip=",nip,") stresses are:"
 WRITE(11,'(A,/,A)')"    Element     x-coord     y-coord     z-coord",    &
  "    sig_x       sig_y       sig_z       tau_xy      tau_yz      tau_zx"
 CALL sample(element,points,weights) 
 xnew(0)=zero
 elements_4: DO iel=1,nels
   props(1)=prop(1,etype(iel)) 
   props(2)=prop(2,etype(iel)) 
   dee=zero
   CALL umat(sigma,statev,dee,sse,spd,scd,rpl,ddsddt,drplde,              &
     drpldt,stran,eld,time,dtime,temp,dtemp,predef,dpred,cmname,ndi,nshr, &
     nst,nstatv,props,nprops,points,drot,pnewdt,celent,dfgrd0,dfgrd1,iel, &
     npt,layer,kspt,kstep,kinc)
   num=g_num(:,iel) 
   coord=TRANSPOSE(g_coord(:,num))
   g=g_g(:,iel) 
   eld=xnew(g)
   gauss_pts_2: DO i=1,nip
     CALL shape_der(der,points,i) 
     CALL shape_fun(fun,points,i)
     gc=MATMUL(fun,coord) 
     jac=MATMUL(der,coord) 
     CALL invert(jac)
     deriv=MATMUL(jac,der) 
     CALL beemat(bee,deriv)
     sigma=MATMUL(dee,MATMUL(bee,eld))
     WRITE(11,'(I8,4X,3E12.4)')iel,gc 
     WRITE(11,'(6E12.4)')sigma
   END DO gauss_pts_2
 END DO elements_4
 WRITE(11,'(/A,F12.4,A)')" Time for setup was                        ",   &
                           timest(2)-timest(1),"s"
 WRITE(11,'(A,F12.4,A)')" Total time for this analysis was          ",    &
                           elap_time()-timest(1),"s"

END SUBROUTINE p57
