SUBROUTINE p104(input_file,output_file)  
!--------------------------------------------------------------------------
! Program 10.4: Eigenvalue analysis of an elastic solid in plane strain
!               using 4-node rectangular quadrilaterals. 
!               Arnoldi's Method. Lumped mass. Element by element. 
!               Mesh numbered in x- or y-direction.
!--------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,ido,iel,ierr,info,iparam(11),ipntr(11),ishfts,iters,j,k,      &
   logfil,lworkl,maxitr,model,msaitr,msapps,msaupd,msaup2,mseigt,mseupd,  &
   msgets,ndigit,nconv,ncv,ndim=2,nev,ndof,nels,neq,nip=4,nlen,nn,nod,    &
   nodof=2,nprops=3,np_types,nr,nst=3,nxe,nye   
 REAL(iwp)::det,one=1.0_iwp,sigma,tol,zero=0.0_iwp
 CHARACTER(len=15)::argv,bmat,element='quadrilateral',which
 LOGICAL::rvec
!--------------------------- dynamic arrays--------------------------------
 REAL(iwp),ALLOCATABLE::bee(:,:),coord(:,:),d(:,:),dee(:,:),der(:,:),     &
   deriv(:,:),diag(:),ecm(:,:),fun(:),g_coord(:,:),jac(:,:),km(:,:),      &
   mm(:,:),pmul(:),points(:,:),prop(:,:),resid(:),udiag(:),utemp(:),      &
   v(:,:),vdiag(:),weights(:),workd(:),workl(:),x_coords(:),y_coords(:)
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g (:,:),g_num(:,:),nf(:,:),num(:)
 LOGICAL,ALLOCATABLE::select(:)
! include 'debug.h'
!----------------------input and initialisation----------------------------
 
  OPEN(10,FILE=input_file)
  OPEN(11,FILE=output_file)
  READ(10,*)nxe,nye,nod,np_types
  CALL mesh_size(element,nod,nels,nn,nxe,nye);ndof=nod*nodof
  ALLOCATE(nf(nodof,nn),points(nip,ndim),dee(nst,nst),g_coord(ndim,nn),   &
    coord(nod,ndim),fun(nod),jac(ndim,ndim), weights(nip),utemp(ndof),    &
    g_num(nod,nels),der(ndim,nod),deriv(ndim,nod),bee(nst,ndof),num(nod), &
    km(ndof,ndof),g(ndof),g_g(ndof,nels),mm(ndof,ndof),ecm(ndof,ndof),    &
    pmul(ndof),x_coords(nxe+1),y_coords(nye+1),prop(nprops,np_types),     &
    etype(nels))
  READ(10,*)prop
  etype=1
  IF(np_types>1)READ(10,*)etype 
  READ(10,*)x_coords,y_coords
  nf=1
  READ(10,*)nr,(k,nf(:,k),i=1,nr)
  CALL formnf(nf)
  neq=MAXVAL(nf)
  READ(10,*)nev,ncv,bmat,which,tol,maxitr
  lworkl=ncv*(ncv+8)
  ndigit=-3
  logfil=6
  msgets=0
  msaitr=0
  msapps=0
  msaupd=2
  msaup2=0
  mseigt=0
  mseupd=0
! msaupd=1
  msaup2=0
  mseigt=0
  mseupd=0
  info=0
  ido=0
  ishfts=1
  model=1
  ipntr=0
  iparam=0
  iparam(1)=ishfts
  iparam(3)=maxitr
  iparam(7)=model
!-----------------------loop the elements to find global array sizes-------
  elements_1: DO iel=1,nels
    CALL geom_rect(element,iel,x_coords,y_coords,coord,num,'y')
    CALL num_to_g (num,nf,g)
    g_num(:,iel)=num
    g_coord(:,num)=transpose(coord)
    g_g(:,iel)=g
  END DO elements_1
  CALL mesh(g_coord,g_num,argv,nlen,12)                                              
  WRITE(11,'(A,I5,A/)')" There are",neq," equations"
  ALLOCATE(v(neq,ncv),diag(neq),udiag(neq),vdiag(neq),workd(3*neq),       &
    resid(neq),workl(lworkl),d(ncv,2),select(ncv))
  diag=zero
  v=zero
  udiag=zero
  vdiag=zero
  workd=zero
  resid=zero
  CALL sample(element,points,weights)
!--------------- element stiffness integration and assembly----------------
 elements_2: DO iel=1,nels
   num=g_num(:,iel)
   coord=transpose(g_coord(:,num ))
   g=g_g(:,iel)
   km=zero
   mm=zero 
   CALL deemat(dee,prop(1,etype(iel)),prop(2,etype(iel)))  
   integrating_pts_1: DO i=1,nip
     CALL shape_fun(fun,points,i)
     CALL shape_der(der,points,i)
     jac=MATMUL(der,coord)
     det=determinant(jac)
     CALL invert(jac) 
     deriv=MATMUL(jac,der)
     CALL beemat(bee,deriv)
     km=km+MATMUL(MATMUL(TRANSPOSE(bee),dee),bee)*det*weights(i)
     CALL ecmat(ecm,fun,ndof,nodof)
     mm=mm+ecm*det*weights(i)**prop(3,etype(iel))
   END DO integrating_pts_1
   DO i=1,ndof
     IF(g(i)/=0)diag(g(i))=diag(g(i))+sum(mm(i,:))
   END DO
 END DO elements_2
!------------------------------find eigenvalues----------------------------
 iters=0
 diag=one/SQRT(diag)    ! diag holds l**(-1/2)
 DO
   iters=iters+1
   CALL dsaupd(ido,bmat,neq,which,nev,tol,resid,ncv,v,neq,iparam,ipntr,   &
     workd,workl,lworkl,info)
   IF(ido/=-1.AND.ido/=1)EXIT
   udiag=zero
   vdiag=workd(ipntr(1):ipntr(1)+neq-1)*diag
   elements_3: DO iel=1,nels
   g=g_g(:,iel)
     DO i=1,ndof
       IF(g(i)==0)pmul(i)=zero
       IF(g(i)/=0)pmul(i)=vdiag(g(i))
     END DO
     utemp=matmul(km,pmul)
     DO i=1,ndof
       IF(g(i)/=0)udiag(g(i))=udiag(g(i))+utemp(i)
     END DO
   END DO elements_3
   udiag=udiag*diag
   workd(ipntr(2):ipntr(2)+neq-1)=udiag
  END DO
  WRITE(11,'(A,I5,A/)')" Convergence after ",iters," iterations"
!-----Either we have convergence or there is an error----------------------
  IF(info<0)THEN
    WRITE(11,*)"Fatal error in ssaupd "
  ELSE
    rvec=.TRUE.
    CALL dseupd(rvec,'All',select,d,v,neq,sigma,bmat,neq,which,nev,tol,   &
      resid,ncv,v,neq,iparam,ipntr,workd,workl,lworkl,ierr)
!------------------------- write out the spectrum -------------------------
    IF(ierr/=0 )THEN
      WRITE(11,*)"Fatal error in sseupd" 
      WRITE(11,*)"Should compute residuals"
    END IF
    WRITE(11,'(A)')"The eigenvalues are:"
    WRITE(11,'(6E12.4)')d(1:nev,1)       
    DO i=1,nev
      udiag(:)=v(1:neq,i)
      udiag=udiag*diag 
      WRITE(11,'(A,I3,A)')" Eigenvector number",i," is:"
      WRITE(11,'(6E12.4)')udiag(1:)/MAXVAL(ABS(udiag(1:)))
      IF(i==1)CALL dismsh((/zero,udiag/),nf,0.1_iwp,g_coord,g_num,argv,nlen,13)
    END DO
  END IF
END SUBROUTINE p104
