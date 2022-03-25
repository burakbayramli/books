SUBROUTINE p44(input_file,output_file)
!-------------------------------------------------------------------------
! Program 4.4 Analysis of elastic rigid-jointed frames using 2-node
!             beam/rod elements in 2- or 3-dimensions.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,k,loaded_nodes,ndim,ndof,nels,neq,nod=2,   &
   nodof,nn,nprops,np_types,nr,nlen
 REAL(iwp)::penalty=1.0e20_iwp,zero=0.0_iwp
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),kdiag(:),nf(:,:), &
   no(:),node(:),num(:),sense(:)
 REAL(iwp),ALLOCATABLE::action(:),coord(:,:),eld(:),gamma(:),g_coord(:,:),&
   km(:,:),kv(:),loads(:),prop(:,:),value(:)
 CHARACTER(LEN=15)::argv
!-----------------------input and initialisation--------------------------
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nels,nn,ndim,nprops,np_types
 IF(ndim==2)nodof=3
 IF(ndim==3)nodof=6
 ndof=nod*nodof
 ALLOCATE(nf(nodof,nn),km(ndof,ndof),coord(nod,ndim),g_coord(ndim,nn),    &
   eld(ndof),action(ndof),g_num(nod,nels),num(nod),g(ndof),gamma(nels),   &
   g_g(ndof,nels),prop(nprops,np_types),etype(nels))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 IF(ndim==3)READ(10,*)gamma
 READ(10,*)g_coord
 READ(10,*)g_num
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf) 
 ALLOCATE(kdiag(neq),loads(0:neq))
!-----------------------loop the elements to find global array sizes------
 kdiag=0
 elements_1: DO iel=1,nels
   num=g_num(:,iel)
   CALL num_to_g(num,nf,g)
   g_g(:,iel)=g
   CALL fkdiag(kdiag,g)
 END DO elements_1
 DO i=2,neq
   kdiag(i)=kdiag(i)+kdiag(i-1)
 END DO
 ALLOCATE(kv(kdiag(neq)))
 WRITE(11,'(2(A,I10))')                                                   &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
!-----------------------global stiffness matrix assembly------------------
 kv=zero  
 elements_2: DO iel=1,nels
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   CALL rigid_jointed(km,prop,gamma,etype,iel,coord)
   g=g_g(:,iel)
   CALL fsparv(kv,km,g,kdiag)
 END DO elements_2
!-----------------------read loads and/or displacements-------------------
 loads=zero
 READ(10,*)loaded_nodes,(k,loads(nf(:,k)),i=1,loaded_nodes)
 READ(10,*)fixed_freedoms
 IF(fixed_freedoms/=0)THEN
   ALLOCATE(node(fixed_freedoms),no(fixed_freedoms),                      &
     sense(fixed_freedoms),value(fixed_freedoms))
   READ(10,*)(node(i),sense(i),value(i),i=1,fixed_freedoms)
   DO i=1,fixed_freedoms
     no(i)=nf(sense(i),node(i))
   END DO
   kv(kdiag(no))=kv(kdiag(no))+penalty
   loads(no)=kv(kdiag(no))*value
 END IF
!-----------------------equation solution --------------------------------
 CALL sparin(kv,kdiag)
 CALL spabac(kv,loads,kdiag)
 loads(0)=zero
 WRITE(11,'(/A)')  "  Node   Displacements and Rotation(s)"
 DO k=1,nn
   WRITE(11,'(I5,6E12.4)')k,loads(nf(:,k))
 END DO   
!-----------------------retrieve element end actions----------------------
 WRITE(11,'(/A)')" Element Actions"
 elements_3: DO iel=1,nels
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   g=g_g(:,iel)
   eld=loads(g)
   CALL rigid_jointed(km,prop,gamma,etype,iel,coord)
   action=MATMUL(km,eld)
   IF(ndim<3)THEN
     WRITE(11,'(I5,6E12.4)')iel,action
   ELSE
     WRITE(11,'(I5,6E12.4)')iel,   action(1: 6)
     WRITE(11,'(A,6E12.4)')"     ",action(7:12)
   END IF
 END DO elements_3
END SUBROUTINE p44

