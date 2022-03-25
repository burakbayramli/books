SUBROUTINE p112(input_file,output_file) 
!-------------------------------------------------------------------------
! Program 11.2 Forced vibration of an elastic solid in plane strain
!              using 4- or 8-node rectangular quadrilaterals. Lumped mass.
!              Mesh numbered in x- or y-direction. Modal superposition.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,idiag,iel,ifail,j,jj,k,nband,ndim=2,ndof,nels,neq,nlen,nmodes,&
   nn,nod,nodof=2,npri,nprops=3,np_types,nr,nres,nstep,nxe,nye
 REAL(iwp)::aa,area,bb,dr,dtim,d4=4.0_iwp,etol=1.e-30_iwp,f,k1,k2,omega,  &
   one=1.0_iwp,penalty=1.0e20_iwp,time,two=2.0_iwp,zero=0.0_iwp
 CHARACTER(LEN=15)::argv,element='quadrilateral'
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g (:,:),g_num(:,:),kdiag(:),nf(:,:),&
   num(:)
 REAL(iwp),ALLOCATABLE::bigk(:,:),coord(:,:),diag(:),g_coord(:,:),kh(:),  &
   km(:,:),ku(:,:),kv(:),mm(:,:),prop(:,:),rrmass(:),udiag(:),xmod(:),    &
   x_coords(:),y_coords(:)
!-----------------------input and initialisation--------------------------
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,nod,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ndof=nod*nodof
 ALLOCATE(nf(nodof,nn),g_coord(ndim,nn),coord(nod,ndim),g_num(nod,nels),  &
   num(nod),km(ndof,ndof),g(ndof),g_g(ndof,nels),x_coords(nxe+1),         &
   y_coords(nye+1),prop(nprops,np_types),etype(nels),mm(ndof,ndof))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)x_coords,y_coords,dtim,nstep,npri,nres,dr
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)
 READ(10,*)nmodes,omega
 ALLOCATE(diag(0:neq),udiag(0:neq),kdiag(neq),rrmass(0:neq),xmod(nmodes), &
   bigk(neq,nmodes))
!-------loop the elements to find nband and set up global arrays----------
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
 DO i=2,neq
   kdiag(i)=kdiag(i)+kdiag(i-1)
 END DO
 WRITE(11,'(A,I5,A,/,A,I5,/,A,I5)')" There are",neq," equations",         &
   " The half-bandwidth (including diagonal) is",nband+1,                 &
   " The skyline storage is",kdiag(neq)
   diag=zero
   bigk=zero
!-----------------------element stiffness and mass assembly---------------
 ALLOCATE(ku(neq,nband+1),kv(kdiag(neq)),kh(kdiag(neq)))
 ku=zero
 elements_2: DO iel=1,nels
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   g=g_g(:,iel)
   CALL rect_km(km,coord,prop(1,etype(iel)),prop(2,etype(iel)))
   CALL formku(ku,km,g)
   area=(MAXVAL(coord(:,1))-MINVAL(coord(:,1)))*    &
                              (MAXVAL(coord(:,2))-MINVAL(coord(:,2)))
   CALL elmat(area,prop(3,etype(iel)),mm)
   CALL formlump(diag,mm,g)
  END DO elements_2
  rrmass(1:)=one/SQRT(diag(1:))
!-----------------------reduce to standard eigenvalue problem-------------
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
!-----------------------extract the "mass normalised" eigenvectors--------
 DO i=1,nmodes
   kv=kh
   kv(kdiag)=kv(kdiag)-diag(i)
   kv(1)=kv(1)+penalty
   udiag=zero
   udiag(1)=kv(1)
   CALL sparin_gauss(kv,kdiag)
   CALL spabac_gauss(kv,udiag,kdiag)
   udiag=rrmass*udiag 
   udiag(1:)=udiag(1:)/SQRT(SUM(udiag(1:)**2/rrmass(1:)**2))
   bigk(:,i)=udiag(1:)
 END DO
 udiag=zero
 time=zero
 WRITE(11,'(/A,I5)')" Result at node",nres
 WRITE(11,'(A)')"    time        load        x-disp      y-disp"
 WRITE(11,'(4E12.4)')time,COS(omega*time),udiag(nf(:,nres))
!------------------------time stepping loop-------------------------------
 DO jj=1,nstep
   time=time+dtim
   DO i=1,nmodes
     f=bigk(neq,i)
!-----------------------analytical solution for cosine loading------------
     k1=diag(i)-omega**2
     k2=k1*k1+d4*omega**2*dr**2*diag(i)
     aa=f*k1/k2
     bb=f*two*omega*dr*SQRT(diag(i))/k2
     xmod(i)=aa*COS(omega*time)+bb*SIN(omega*time)
   END DO
!-----------------------superpose the modes-------------------------------
   udiag(1:)=MATMUL(bigk,xmod(1:))
   IF(jj/npri*npri==jj)WRITE(11,'(4E12.4)')time,COS(omega*time),          &
     udiag(nf(:,nres))
 END DO

END SUBROUTINE p112
