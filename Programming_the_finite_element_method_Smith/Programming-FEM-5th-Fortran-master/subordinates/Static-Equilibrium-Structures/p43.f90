SUBROUTINE p43(input_file,output_file)
!-------------------------------------------------------------------------
! Program 4.3 Analysis of elastic beams using 2-node beam elements
!             (elastic foundation optional).
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,k,loaded_nodes,ndof=4,nels,neq,nod=2,      &
   nodof=2,nn,nprops,np_types,nr,nlen
 REAL(iwp)::penalty=1.0e20_iwp,zero=0.0_iwp
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),kdiag(:),nf(:,:),no(:),      &
   node(:),num(:),sense(:) 
 REAL(iwp),ALLOCATABLE::action(:),eld(:),ell(:),km(:,:),kv(:),loads(:),   &
   mm(:,:),prop(:,:),value(:)
 CHARACTER(LEN=15)::argv
!-----------------------input and initialisation--------------------------
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nels,nprops,np_types
 nn=nels+1
 ALLOCATE(g(ndof),num(nod),nf(nodof,nn),etype(nels),ell(nels),eld(ndof),  &
   km(ndof,ndof),mm(ndof,ndof),action(ndof),g_g(ndof,nels),               &
   prop(nprops,np_types))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)ell
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)  
 ALLOCATE(kdiag(neq),loads(0:neq))
!-----------------------loop the elements to find global array sizes------
 kdiag=0
 elements_1: DO iel=1,nels
   num=(/iel,iel+1/)
   CALL num_to_g(num,nf,g)
   g_g(:,iel)=g
   CALL fkdiag(kdiag,g)
 END DO elements_1
 DO i=2,neq
   kdiag(i)=kdiag(i)+kdiag(i-1)
 END DO
 ALLOCATE(kv(kdiag(neq)))
 WRITE(11,'(2(A,I5))')                                                    &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
!-----------------------global stiffness matrix assembly------------------
 kv=zero   
 elements_2: DO iel=1, nels
   CALL beam_km(km,prop(1,etype(iel)),ell(iel))
   mm=zero
   IF(nprops>1)CALL beam_mm(mm,prop(2,etype(iel)),ell(iel))
   g=g_g(:,iel)
   CALL fsparv(kv,km+mm,g,kdiag)
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
 WRITE(11,'(/A)')"  Node Translation Rotation"
 DO k=1,nn
   WRITE(11,'(I5,2E12.4)')k,loads(nf(:,k))
 END DO    
!-----------------------retrieve element end actions----------------------
 WRITE(11,'(/A)')" Element Force       Moment      Force       Moment"
 elements_3: DO iel=1,nels
   CALL beam_km(km,prop(1,etype(iel)),ell(iel))
   mm=zero
   IF(nprops>1)CALL beam_mm(mm,prop(2,etype(iel)),ell(iel))
   g=g_g(:,iel)
   eld=loads(g)
   action=MATMUL(km+mm,eld)
   WRITE(11,'(I5,4E12.4)')iel,action
 END DO elements_3
 CALL beamdis(loads,nf,0.40_iwp,10,nels,ell,argv,nlen,12)
END SUBROUTINE p43

