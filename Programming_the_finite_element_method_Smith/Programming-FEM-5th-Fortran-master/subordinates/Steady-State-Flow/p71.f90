SUBROUTINE p71(input_file,output_file) 
!-------------------------------------------------------------------------
! SUBROUTINE 7.1 One dimensional analysis of steady seepage using
!             2-node "rod" elements.
!-------------------------------------------------------------------------
 USE main
 IMPLICIT NONE 
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,k,loaded_nodes,nels,neq,nlen,nod=2,nn,     &
   nprops=1,np_types
 REAL(iwp)::penalty=1.0e20_iwp,zero=0.0_iwp
 CHARACTER(LEN=15)::argv
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),kdiag(:),g_num(:,:),node(:),num(:) 
 REAL(iwp),ALLOCATABLE::disps(:),ell(:),kp(:,:),kv(:),kvh(:),loads(:),    &
   prop(:,:),value(:)
!-----------------------input and initialisation--------------------------
 
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nels,nn,np_types
 neq=nn
 ALLOCATE(ell(nels),num(nod),prop(nprops,np_types),etype(nels),           &
   kp(nod,nod),g_num(nod,nels),kdiag(neq),loads(0:neq),disps(0:neq))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)ell
 READ(10,*)g_num
 kdiag=0
!-----------------------loop the elements to find global arrays sizes-----
 elements_1: DO iel=1,nels
   num=g_num(:,iel)
   CALL fkdiag(kdiag,num) 
 END DO elements_1
 DO i=2,neq
   kdiag(i)=kdiag(i)+kdiag(i-1)
 END DO
 WRITE(11,'(2(A,I5))')                                                    &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
 ALLOCATE(kv(kdiag(neq)),kvh(kdiag(neq)))
 kv=zero
!-----------------------global conductivity matrix assembly---------------
 elements_2: DO iel=1,nels
   CALL rod_km(kp,prop(1,etype(iel)),ell(iel))
   num=g_num(:,iel)
   CALL fsparv(kv,kp,num,kdiag)
 END DO elements_2
 kvh=kv
!-----------------------specify boundary values---------------------------
 loads=zero
 READ(10,*)loaded_nodes,(k,loads(k),i=1,loaded_nodes)   
 READ(10,*)fixed_freedoms 
 IF(fixed_freedoms/=0)THEN
   ALLOCATE(node(fixed_freedoms),value(fixed_freedoms))
   READ(10,*)(node(i),value(i),i=1,fixed_freedoms)
   kv(kdiag(node))=kv(kdiag(node))+penalty
   loads(node)=kv(kdiag(node))*value
 END IF
!-----------------------equation solution---------------------------------
 CALL sparin(kv,kdiag)
 CALL spabac(kv,loads,kdiag)
!-----------------------retrieve nodal net flow rates---------------------
 CALL linmul_sky(kvh,loads,disps,kdiag)
 WRITE(11,'(/A)')"  Node Total Head  Flow rate"
 disps(0)=zero
 DO k=1,nn
   WRITE(11,'(I5,2E12.4)')k,loads(k),disps(k)
 END DO
 WRITE(11,'(/A)')"       Inflow      Outflow"
 WRITE(11,'(5X,2E12.4)')                                                  &
   SUM(disps,MASK=disps>zero),SUM(disps,MASK=disps<zero)

END SUBROUTINE p71
