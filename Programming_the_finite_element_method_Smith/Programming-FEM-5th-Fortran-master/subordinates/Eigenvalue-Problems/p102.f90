SUBROUTINE p102(input_file,output_file)
!-------------------------------------------------------------------------
! Program 10.2 Eigenvalue analysis of an elastic solid in plane strain
!              using 4- or 8-node rectangular quadrilaterals. Lumped mass.
!              Mesh numbered in x- or y-direction.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,idiag,iel,ifail,j,k,nband,ndim=2,ndof,nels,neq,nmodes,nn,nod, &
   nodof=2,nlen,nprops=3,np_types,nr,nxe,nye   
 REAL(iwp)::area,etol=1.e-30_iwp,one=1.0_iwp,penalty=1.e20_iwp,zero=0.0_iwp
 CHARACTER(LEN=15)::argv,element='quadrilateral'
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g (:,:),g_num(:,:),kdiag(:),nf(:,:),&
   num(:)
 REAL(iwp),ALLOCATABLE::coord(:,:),diag(:),g_coord(:,:),kh(:),km(:,:),    &
   ku(:,:),kv(:),mm(:,:),prop(:,:),rrmass(:),udiag(:),x_coords(:),        &
   y_coords(:)
!-----------------------input and initialisation--------------------------

 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,nod,np_types 
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ndof=nod*nodof
 ALLOCATE(nf(nodof,nn),g_coord(ndim,nn),coord(nod,ndim),mm(ndof,ndof),    &
   g_num(nod,nels),num(nod),km(ndof,ndof),g(ndof),g_g(ndof,nels),         &
   prop(nprops,np_types),x_coords(nxe+1),y_coords(nye+1),etype(nels))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)x_coords,y_coords
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)
 ALLOCATE(diag(0:neq),udiag(0:neq),kdiag(neq),rrmass(0:neq))
!-----------------------loop the elements to find global array sizes------
 nband=0
 kdiag=0
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,'y')   
   CALL num_to_g(num,nf,g)
   g_num(:,iel)=num 
   g_coord(:,num)=TRANSPOSE(coord)
   g_g(:,iel)=g
   CALL fkdiag(kdiag,g)
   IF(nband<bandwidth(g))nband=bandwidth(g)
 END DO elements_1
 CALL mesh(g_coord,g_num,argv,nlen,12) 
 DO i=2,neq
   kdiag(i)=kdiag(i)+kdiag(i-1)
 END DO
 WRITE(11,'(A,I5,A,/,A,I5,/,A,I5)')" There are",neq," equations",         &
   " The half-bandwidth (including diagonal) is",nband+1,                 &
   " The skyline storage is",kdiag(neq)
!-----------------------global stiffness and mass matrix assembly---------
 ALLOCATE(ku(neq,nband+1),kv(kdiag(neq)),kh(kdiag(neq)))
 diag=zero
 ku=zero
 elements_2: DO iel=1,nels
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   g=g_g(:,iel)
   CALL rect_km(km,coord,prop(1,etype(iel)),prop(2,etype(iel)))
   CALL formku(ku,km,g)
   area=(MAXVAL(coord(:,1))-MINVAL(coord(:,1)))*                          &
        (MAXVAL(coord(:,2))-MINVAL(coord(:,2)))
   CALL elmat(area,prop(3,etype(iel)),mm)
   CALL formlump(diag,mm,g)
 END DO elements_2
!-----------------------reduce to standard eigenvalue problem-------------
 rrmass(1:)=one/SQRT(diag(1:))
 DO i=1,neq
 IF(i<=neq-nband)THEN
   k=nband+1
 ELSE
   k=neq-i+1
 END IF
   DO j=1,k
     ku(i,j)=ku(i,j)*rrmass(i)*rrmass(i+j-1)
   END DO
 END DO
!-----------------------convert to skyline form---------------------------
 kh(1)=ku(1,1)
 k=1
 DO i=2,neq
   idiag=kdiag(i)-kdiag(i-1)
   DO j=1,idiag
     k=k+1
     kh(k)=ku(i+j-idiag,1-j+idiag)
   END DO
 END DO
!-----------------------extract the eigenvalues---------------------------
 CALL bandred(ku,diag,udiag)
 ifail=1
 CALL bisect(diag,udiag,etol,ifail)
 WRITE(11,'(/A)')" The eigenvalues are:"
 WRITE(11,'(6E12.4)')diag(1:)
!-----------------------extract the eigenvectors--------------------------
 READ(10,*)nmodes
 DO i=1,nmodes
   kv=kh;kv(kdiag)=kv(kdiag)-diag(i)
   kv(1)=kv(1)+penalty
   udiag=zero
   udiag(1)=kv(1)
   CALL sparin_gauss(kv,kdiag)
   CALL spabac_gauss(kv,udiag,kdiag)
   udiag=rrmass*udiag
   WRITE(11,'(A,I3,A)')" Eigenvector number",i," is:"
   WRITE(11,'(6E12.4)')udiag(1:)/MAXVAL(ABS(udiag(1:)))
   IF(i==1)CALL dismsh(udiag,nf,0.1_iwp,g_coord,g_num,argv,nlen,13)
 END DO
END SUBROUTINE p102
