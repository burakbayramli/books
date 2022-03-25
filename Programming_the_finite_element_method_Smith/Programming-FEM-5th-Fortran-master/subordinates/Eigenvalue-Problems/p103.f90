SUBROUTINE p103(input_file,output_file)
!-------------------------------------------------------------------------
! Program 10.3: Eigenvalue analysis of an elastic solid in plane strain
!               using 4-node rectangular quadrilaterals. Lanczos Method.
!               Consistent mass. Mesh numbered in y-direction.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,iel,iflag=-1,iters,jflag,k,lalfa,leig,lp=6,lx,lz,nband=0,     &
   ndim=2,ndof,neig=0,nels,neq,nip=4,nlen,nmodes,nn,nod,nodof=2,nprops=3, &
   np_types,nr,nst=3,nxe,nye
 REAL(iwp)::acc,det,el,er,zero=0.0_iwp
 CHARACTER(LEN=15)::argv,element='quadrilateral'
!----------------------------- dynamic arrays-----------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),jeig(:,:),nf(:,:),&
   nu(:),num(:)
 REAL(iwp),ALLOCATABLE::alfa(:),bee(:,:),beta(:),coord(:,:),dee(:,:),     &
   del(:),der(:,:),deriv(:,:),diag(:),ecm(:,:),eig(:),fun(:),g_coord(:,:),&
   jac(:,:),kb(:,:),km(:,:),mb(:,:),mm(:,:),points(:,:),prop(:,:),ua(:),  &
   udiag(:),va(:),v_store(:,:),weights(:),w1(:),x(:),x_coords(:),y(:,:),  &
   y_coords(:),z(:,:)
!---------------------------input and initialisation----------------------

 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,nod,np_types 
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ndof=nod*nodof
 ALLOCATE(nf(nodof,nn),points(nip,ndim),dee(nst,nst),g_coord(ndim,nn),    &
   coord(nod,ndim),fun(nod),jac(ndim,ndim),weights(nip),g_num(nod,nels),  &
   der(ndim,nod),deriv(ndim,nod),bee(nst,ndof),num(nod),km(ndof,ndof),    &
   g(ndof),g_g(ndof,nels),mm(ndof,ndof),ecm(ndof,ndof),                   &
   prop(nprops,np_types),x_coords(nxe+1),y_coords(nye+1),etype(nels))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype 
 READ(10,*)x_coords,y_coords
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)
 READ(10,*)nmodes,el,er,lalfa,leig,lx,lz,acc
 ALLOCATE(eig(leig),x(lx),del(lx),nu(lx),jeig(2,leig),alfa(lalfa),        &
   beta(lalfa),z(lz,leig))
!-------- loop the elements to find nband and set up global arrays -------
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,'y')
   CALL num_to_g(num,nf,g)
   g_num(:,iel)=num
   g_coord(:,num)=TRANSPOSE(coord)
   g_g(:,iel)=g
   IF(nband<bandwidth(g))nband=bandwidth(g)
 END DO elements_1
 WRITE(11,'(A,I5,A,/,A,I5,/,A,I5)')" There are",neq," equations",         &
   " The half-bandwidth (including diagonal) is",nband+1
 ALLOCATE(kb(neq,nband+1),mb(neq,nband+1),ua(0:neq),va(0:neq),            &
   diag(0:neq),udiag(0:neq),w1(0:neq),y(0:neq,leig),v_store(0:neq,lalfa))
 kb=zero
 mb=zero
 ua=zero
 va=zero
 eig=zero
 jeig=0
 x=zero
 del=zero
 nu=0
 alfa=zero
 beta=zero
 diag=zero
 udiag=zero
 w1=zero
 y=zero
 z=zero
 CALL sample(element,points,weights)
!----------------- element stiffness integration and assembly-------------
 elements_2: DO iel=1,nels
   CALL deemat(dee,prop(1,etype(iel)),prop(2,etype(iel)))
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   g=g_g(:,iel)
   km=zero
   mm=zero
   integrating_pts_1: DO i=1,nip
     CALL shape_fun(fun,points,i)
     CALL shape_der(der,points,i)
     jac=MATMUL(der,coord)
     det=determinant(jac)
     CALL invert(jac)
     deriv=MATMUL(jac,der)
     call beemat(bee,deriv)
     km=km+MATMUL(MATMUL(TRANSPOSE(bee),dee),bee)*det*weights(i)
     call ecmat(ecm,fun,ndof,nodof) 
     mm=mm+ecm*det*weights(i)*prop(3,etype(iel))
   END DO integrating_pts_1
   CALL formkb(kb,km,g)
   CALL formkb(mb,mm,g)
 END DO elements_2
 CALL cholin(mb)
!------------------------------find eigenvalues---------------------------
 DO iters=1,lalfa
   CALL lancz1(neq,el,er,acc,leig,lx,lalfa,lp,iflag,ua,va,eig,jeig,neig,x,&
     del,nu,alfa,beta,v_store)
   IF(iflag==0)EXIT
   IF(iflag>1)THEN
     WRITE(11,'(A,I5)')                                                   &
       " Lancz1 is signalling failure, with iflag = ",   iflag
     STOP
   END IF
!--- iflag = 1 therefore form u + a * v  ( candidate for ebe ) -----------
   udiag=va
   CALL chobk2(mb,udiag)
   CALL banmul(kb,udiag,diag) 
   CALL chobk1(mb,diag)
   ua=ua+diag
 END DO
!--- iflag = 0 therefore write out the spectrum  -------------------------
 WRITE(11,'(A,I5,A/)')" It took",iters," iterations"
 WRITE(11,'(3(A,E12.4))')" Eigenvalues in the range",el," and",er," are:"
 WRITE(11,'(6E12.4)')eig(1:neig)
!------------------- calculate the eigenvectors --------------------------
 IF(neig>10)neig=10
  CALL lancz2(neq,lalfa,lp,eig,jeig,neig,alfa,beta,lz,jflag,y,w1,z,v_store)    
!-------------------if jflag is zero  calculate the eigenvectors ---------
 IF(jflag==0)THEN
   DO i=1,nmodes
     udiag(:)=y(:,i)
     CALL chobk2(mb,udiag)
     WRITE(11,'(" Eigenvector number",I4," is:")')i
     WRITE(11,'(6E12.4)')udiag(1:)/MAXVAL(ABS(udiag(1:)))
   END DO
 ELSE
! lancz2 fails
   WRITE(11,'(A,I5)')" Lancz2 is signalling failure with jflag = ",jflag
 END IF
END SUBROUTINE p103
