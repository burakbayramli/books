SUBROUTINE p41(input_file,output_file)
    !-------------------------------------------------------------------------
    ! Program 4.1 One dimensional analysis of axially loaded elastic rods
    !             using 2-node rod elements.
    !-------------------------------------------------------------------------
    USE main
    USE geom
    IMPLICIT NONE
    CHARACTER(len=60),INTENT(IN) :: input_file
    CHARACTER(len=60),INTENT(OUT) :: output_file
    INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
    INTEGER::fixed_freedoms,i,iel,k,loaded_nodes,ndof=2,nels,neq,nlen,nod=2, &
    nodof=1,nn,nprops=1,np_types,nr
    REAL(iwp)::penalty=1.0e20_iwp,zero=0.0_iwp
    ! CHARACTER(LEN=15)::argv
    CHARACTER(LEN=30)::file_p41
    !-----------------------dynamic arrays------------------------------------
    INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),kdiag(:),nf(:,:),no(:),      &
    node(:),num(:) 
    REAL(iwp),ALLOCATABLE::action(:),eld(:),ell(:),km(:,:),kv(:),loads(:),   &
    prop(:,:),value(:)
    !-----------------------input and initialisation--------------------------
    !CALL getname(argv,nlen)
    !OPEN(10,FILE=input_file)
    !OPEN(11,FILE=output_file)
    !OPEN(10, FILE='.//subordinates//Coupled-Problems//p41_1.dat')
    !OPEN(11, FILE='.//subordinates//Coupled-Problems//p41_1.res')
    OPEN(10, FILE=input_file)
    OPEN(11, FILE=output_file)
    READ(10,*)nels,np_types
    nn=nels+1
    ALLOCATE(g(ndof),num(nod),nf(nodof,nn),etype(nels),ell(nels),eld(ndof),  &
    km(ndof,ndof),action(ndof),g_g(ndof,nels),prop(nprops,np_types))
    READ(10,*)prop
    etype=1
    IF(np_types>1)READ(10,*)etype
    READ(10,*)ell
    nf=1
    READ(10,*)nr,(k,nf(:,k),i=1,nr)
    CALL formnf(nf)
    neq=MAXVAL(nf) 
    ALLOCATE(kdiag(neq),loads(0:neq))
    kdiag=0
    !-----------------------loop the elements to find global arrays sizes-----
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
    elements_2: DO iel=1,nels
    CALL rod_km(km,prop(1,etype(iel)),ell(iel))
    g=g_g(:,iel)
    CALL fsparv(kv,km,g,kdiag)
    END DO elements_2
    !-----------------------read loads and/or displacements-------------------
    loads=zero
    READ(10,*)loaded_nodes,(k,loads(nf(:,k)),i=1,loaded_nodes)
    READ(10,*)fixed_freedoms
    IF(fixed_freedoms/=0)THEN
    ALLOCATE(node(fixed_freedoms),no(fixed_freedoms),value(fixed_freedoms))
    READ(10,*)(node(i),value(i),i=1,fixed_freedoms)
    DO i=1,fixed_freedoms
        no(i)=nf(1,node(i))
    END DO
    kv(kdiag(no))=kv(kdiag(no))+penalty
    loads(no)=kv(kdiag(no))*value 
    END IF
    !-----------------------equation solution -------------------------------- 
    CALL sparin(kv,kdiag)
    CALL spabac(kv,loads,kdiag)
    loads(0)=zero
    WRITE(11,'(/A)')"  Node   Disp"
    DO k=1,nn
    WRITE(11,'(I5,2E12.4)')k,loads(nf(:,k))
    END DO
    !-----------------------retrieve element end actions----------------------
    WRITE(11,'(/A)')" Element Actions"
    elements_3: DO iel=1,nels
    CALL rod_km(km,prop(1,etype(iel)),ell(iel))
    g=g_g(:,iel)
    eld=loads(g)
    action=MATMUL(km,eld)
    WRITE(11,'(I5,2E12.4)')iel,action
    END DO elements_3
END SUBROUTINE
