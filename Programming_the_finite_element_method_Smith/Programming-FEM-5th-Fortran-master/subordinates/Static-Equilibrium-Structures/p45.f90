SUBROUTINE p45(input_file,output_file)
!-------------------------------------------------------------------------
! Program 4.5 Analysis of elasto-plastic beams or rigid-jointed frames
!             using 2-node beam or beam/rod elements in 1-, 2- or
!             3-dimensions
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,iel,incs,iters,iy,k,limit,loaded_nodes,ndim,ndof,nels,neq,    &
   nod=2,nodof,nn,nprops,np_types,nr,nlen
 REAL(iwp)::tol,total_load,zero=0.0_iwp
 LOGICAL::converged
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),kdiag(:),nf(:,:), &
  node(:),num(:)
 REAL(iwp),ALLOCATABLE::action(:),bdylds(:),coord(:,:),dload(:),eld(:),   &
   eldtot(:),gamma(:),g_coord(:,:),holdr(:,:),km(:,:),kv(:),loads(:),     &
   oldis(:),prop(:,:),react(:),val(:,:)
 CHARACTER(LEN=15)::argv
!-----------------------input and initialisation--------------------------
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nels,nn,ndim,nprops,np_types
 IF(ndim==1)nodof=2
 IF(ndim==2)nodof=3
 IF(ndim==3)nodof=6
 ndof=nod*nodof
 ALLOCATE(nf(nodof,nn),km(ndof,ndof),coord(nod,ndim),g_coord(ndim,nn),    &
   eld(ndof),action(ndof),g_num(nod,nels),num(nod),g(ndof),gamma(nels),   &
   g_g(ndof,nels),holdr(ndof,nels),react(ndof),prop(nprops,np_types),     &
   etype(nels))
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
 ALLOCATE(kdiag(neq),loads(0:neq),eldtot(0:neq),bdylds(0:neq),oldis(0:neq))
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
 WRITE(11,'(2(A,I5))')                                                    &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
 holdr=zero
!-----------------------global stiffness matrix assembly------------------
 kv=zero
 elements_2: DO iel=1,nels
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   CALL rigid_jointed(km,prop,gamma,etype,iel,coord)
   g=g_g(:,iel)
   CALL fsparv(kv,km,g,kdiag)
 END DO elements_2
 READ(10,*)loaded_nodes
 ALLOCATE(node(loaded_nodes),val(loaded_nodes,nodof))
 READ(10,*)(node(i),val(i,:),i=1,loaded_nodes)
 READ(10,*)limit,tol,incs
 ALLOCATE(dload(incs))  
 READ(10,*)dload
!-----------------------equation factorisation----------------------------
 CALL sparin(kv,kdiag)
!-----------------------load increment loop-------------------------------
 total_load=zero
 eldtot=zero
 load_incs: DO iy=1,incs
   total_load=total_load+dload(iy)
   WRITE(*,'(/,A,I5)')" load step",iy
   WRITE(11,'(/A,I3,A,E12.4)')" Load step",iy,"    Load factor ",total_load
   oldis=zero
   bdylds=zero
   iters=0
   its: DO
     iters=iters+1
     WRITE(*,*)"iteration no",iters
     loads=zero
     DO i=1,loaded_nodes
       loads(nf(:,node(i)))=dload(iy)*val(i,:)
     END DO
     loads=loads+bdylds
     bdylds=zero
!-----------------------forward/back-substitution-------------------------
     CALL spabac(kv,loads,kdiag)
     loads(0)=zero
!-----------------------check convergence---------------------------------
     CALL checon(loads,oldis,tol,converged)
!-----------------------inspect moments in all elements-------------------
     elements_3: DO iel=1,nels
       num=g_num(:,iel)
       coord=TRANSPOSE(g_coord(:,num))
       g=g_g(:,iel)
       eld=loads(g)
       CALL rigid_jointed(km,prop,gamma,etype,iel,coord)
       action=MATMUL(km,eld)
       react=zero
!------------------if plastic moments exceeded generate correction vector-
       IF(limit/=1)THEN
         CALL hinge(coord,holdr,action,react,prop,iel,etype,gamma)
         bdylds(g)=bdylds(g)-react
         bdylds(0)=zero
       END IF
!-----------------------at convergence update element reactions-----------
       IF(iters==limit.OR.converged)                                      &
         holdr(:,iel)=holdr(:,iel)+react(:)+action(:)
     END DO elements_3
     IF(iters==limit.OR.converged)EXIT its
   END DO its
   eldtot=loads+eldtot
   WRITE(11,'(A)')  "  Node   Displacement(s) and Rotation(s)"
   DO i=1,loaded_nodes
     WRITE(11,'(I5,6E12.4)')node(i),eldtot(nf(:,node(i)))
   END DO
   WRITE(11,'(A,I5,A)')" Converged in",iters," iterations"
   IF(iters==limit.AND.limit/=1)EXIT load_incs
 END DO load_incs
END SUBROUTINE p45
