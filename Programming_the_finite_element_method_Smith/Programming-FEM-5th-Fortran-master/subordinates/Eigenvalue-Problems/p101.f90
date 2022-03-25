SUBROUTINE p101(input_file,output_file)
!-------------------------------------------------------------------------
! Program 10.1 Eigenvalue analysis of elastic beams using 2-node
!              beam elements. Lumped mass.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,idiag,iel,ifail,j,k,nband,ndof=4,nels,neq,nlen,nmodes,nn,     &
   nod=2,nodof=2,nprops=2,np_types,nr
 REAL(iwp)::d12=12.0_iwp,one=1.0_iwp,pt5=0.5_iwp,penalty=1.e20_iwp,       &
   etol=1.0e-30_iwp,zero=0.0_iwp
 CHARACTER(LEN=15)::argv
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),kdiag(:),nf(:,:),num(:)
 REAL(iwp),ALLOCATABLE::diag(:),ell(:),kh(:),km(:,:),ku(:,:),kv(:),       &
   mm(:,:),prop(:,:),rrmass(:),udiag(:)
!-----------------------input and initialisation--------------------------

 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nels,np_types
 nn=nels+1
 ALLOCATE(nf(nodof,nn),km(ndof,ndof),num(nod),g(ndof),mm(ndof,ndof),      &
   ell(nels),etype(nels),g_g(ndof,nels),prop(nprops,np_types))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)ell
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)
 ALLOCATE(diag(0:neq),udiag(0:neq),kdiag(neq),rrmass(0:neq))
!-----------------------loop the elements to find global array sizes------
 nband=0
 kdiag=0
 elements_1: DO iel=1,nels
   num=(/iel,iel+1/)
   CALL num_to_g(num,nf,g)
   g_g(:,iel)=g
   IF(nband<bandwidth(g))nband=bandwidth(g)
   CALL fkdiag(kdiag,g)
 END DO elements_1
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
   g=g_g(:,iel)
   mm=zero
   mm(1,1)=pt5*prop(2,etype(iel))*ell(iel)
   mm(3,3)=mm(1,1)
   mm(2,2)=mm(1,1)*ell(iel)**2/d12
   mm(4,4)=mm(2,2)
   CALL formlump(diag,mm,g)
   CALL beam_km(km,prop(1,etype(iel)),ell(iel))
   CALL formku(ku,km,g)
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
 END DO
END SUBROUTINE p101
