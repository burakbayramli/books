SUBROUTINE p42(input_file,output_file)
!-------------------------------------------------------------------------
! Program 4.2 Analysis of elastic pin-jointed frames using 2-node rod
!             elements in 2- or 3-dimensions
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,k,loaded_nodes,ndim,ndof=2,nels,neq,nod=2, &
   nodof,nn,nprops=1,np_types,nr,nlen
 REAL(iwp)::axial,penalty=1.0e20_iwp,zero=0.0_iwp
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),kdiag(:),nf(:,:), &
   no(:),node(:),num(:),sense(:)
 REAL(iwp),ALLOCATABLE::action(:),coord(:,:),eld(:),g_coord(:,:),km(:,:), &
   kv(:),loads(:),prop(:,:),value(:)
 CHARACTER(LEN=15)::argv
!-----------------------input and initialisation--------------------------
! CALL getname(argv,nlen)
 OPEN(12,FILE=input_file)
 OPEN(13,FILE=output_file)
 READ(12,*)nels,nn,ndim,np_types
 nodof=ndim
 ndof=nod*nodof
 ALLOCATE(nf(nodof,nn),km(ndof,ndof),coord(nod,ndim),g_coord(ndim,nn),    &
   eld(ndof),action(ndof),g_num(nod,nels),num(nod),g(ndof),g_g(ndof,nels),&
   etype(nels),prop(nprops,np_types))
 READ(12,*)prop
 etype=1
 IF(np_types>1)READ(12,*)etype
 READ(12,*)g_coord
 READ(12,*)g_num 
 nf=1
 READ(12,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)  
 ALLOCATE(kdiag(neq),loads(0:neq))
!----------------------loop the elements to find global array sizes-------
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
 WRITE(13,'(2(A,I5))')                                                    &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
!-----------------------global stiffness matrix assembly------------------
 kv=zero     
 elements_2: DO iel=1,nels
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   CALL pin_jointed(km,prop(1,etype(iel)),coord)
   g=g_g(:,iel)
   CALL fsparv(kv,km,g,kdiag)
 END DO elements_2
!-----------------------read loads and/or displacements-------------------
 loads=zero
 READ(12,*)loaded_nodes,(k,loads(nf(:,k)),i=1,loaded_nodes)
 READ(12,*)fixed_freedoms
 IF(fixed_freedoms/=0)THEN
   ALLOCATE(node(fixed_freedoms),no(fixed_freedoms),                      &
     sense(fixed_freedoms),value(fixed_freedoms))
   READ(12,*)(node(i),sense(i),value(i),i=1,fixed_freedoms)
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
 WRITE(13,'(/A)')  "  Node   Displacement(s)"
 DO k=1,nn
   WRITE(13,'(I5,3E12.4)')k,loads(nf(:,k))
 END DO   
!-----------------------retrieve element end actions----------------------
 WRITE(13,'(/A)')" Element Actions"
 elements_3: DO iel=1,nels
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   g=g_g(:,iel)
   eld=loads(g)
   CALL pin_jointed(km,prop(1,etype(iel)),coord)
   action=MATMUL(km,eld)
   WRITE(13,'(I5,6E12.4)')iel,action
   CALL glob_to_axial(axial,action,coord)
   WRITE(13,'(A,E12.4)')"       Axial force =",axial
 END DO elements_3
END SUBROUTINE p42

