SUBROUTINE p74(input_file,output_file) 
!-------------------------------------------------------------------------
! SUBROUTINE 7.4 General two- (plane) or three-dimensional analysis of steady
!             seepage.
!-------------------------------------------------------------------------
 USE main; 
 USE geom; 
 IMPLICIT NONE 
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file    
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,k,loaded_nodes,nci,ndim,nels,neq,nip,nlen, &
   nod,nn,np_types; 
   CHARACTER(LEN=15)::argv,element
 REAL(iwp)::det,penalty=1.0e20_iwp,zero=0.0_iwp
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g_num(:,:),kdiag(:),node(:),num(:)
 REAL(iwp),ALLOCATABLE::coord(:,:),der(:,:),deriv(:,:),disps(:),          &
   g_coord(:,:),jac(:,:),kay(:,:),kp(:,:),kv(:),kvh(:),loads(:),          &
   points(:,:),prop(:,:),value(:),weights(:)
!-----------------------input and initialisation--------------------------
 
 OPEN(10,FILE=input_file); 
 OPEN(11,FILE=output_file)
 READ(10,*)element,nod,nels,nn,nip,ndim,np_types; 
 neq=nn
 ALLOCATE(points(nip,ndim),g_coord(ndim,nn),coord(nod,ndim),etype(nels),  &
   jac(ndim,ndim),weights(nip),num(nod),g_num(nod,nels),der(ndim,nod),    &
   deriv(ndim,nod),kp(nod,nod),kay(ndim,ndim),prop(ndim,np_types),        &
   kdiag(neq),loads(0:neq),disps(0:neq))
 READ(10,*)prop; 
 etype=1; 
 IF(np_types>1)READ(10,*)etype
 READ(10,*)g_coord; 
 READ(10,*)g_num
 IF(ndim==2)CALL mesh(g_coord,g_num,argv,nlen,12); 
 kdiag=0
!-----------------------loop the elements to find global arrays sizes-----
 elements_1: DO iel =1,nels; 
   num=g_num(:,iel); 
   CALL fkdiag(kdiag,num) 
 END DO elements_1
 DO i=2,neq; 
   kdiag(i)=kdiag(i)+kdiag(i-1); 
 END DO
 WRITE(11,'(2(A,I5))')                                                    &
  " There are",neq," equations and the skyline storage is",kdiag(neq)
 ALLOCATE(kv(kdiag(neq)),kvh(kdiag(neq))); 
 kv=zero
 CALL sample(element,points,weights)   
!-----------------------global conductivity matrix assembly---------------
 elements_2: DO iel=1,nels
   kay=zero; 
   DO i=1,ndim; 
     kay(i,i)=prop(i,etype(iel)); 
   END DO
   num=g_num(:,iel); 
   coord=TRANSPOSE(g_coord(:,num)); 
   kp=zero
   gauss_pts_1: DO i=1,nip
     CALL shape_der(der,points,i); 
     jac=MATMUL(der,coord)
     det=determinant(jac); 
     CALL invert(jac); 
     deriv=MATMUL(jac,der)
     kp=kp+MATMUL(MATMUL(TRANSPOSE(deriv),kay),deriv)*det*weights(i)
   END DO gauss_pts_1; 
   CALL fsparv(kv,kp,num,kdiag)
 END DO elements_2; 
 kvh=kv     
!-----------------------specify boundary values---------------------------
 loads=zero; 
 READ(10,*)loaded_nodes,(k,loads(k),i=1,loaded_nodes)   
 READ(10,*)fixed_freedoms 
 IF(fixed_freedoms/=0)THEN
   ALLOCATE(node(fixed_freedoms),value(fixed_freedoms))
   READ(10,*)(node(i),value(i),i=1,fixed_freedoms)
   kv(kdiag(node))=kv(kdiag(node))+penalty
   loads(node)=kv(kdiag(node))*value
 END IF
!-----------------------equation solution---------------------------------
 CALL sparin(kv,kdiag); 
 CALL spabac(kv,loads,kdiag)
!-----------------------retrieve nodal net flow rates---------------------
 CALL linmul_sky(kvh,loads,disps,kdiag) 
 WRITE(11,'(/A)')"  Node Total Head  Flow rate"
 DO k=1,nn; 
   WRITE(11,'(I5,2E12.4)')k,loads(k),disps(k); 
 END DO 
 disps(0)=zero; 
 WRITE(11,'(/A)')"       Inflow      Outflow"
 WRITE(11,'(5X,2E12.4)')                                                  &
   SUM(disps,MASK=disps>zero),SUM(disps,MASK=disps<zero)
 IF(ndim==2.AND.nod==4)THEN
   READ(10,*)nci; 
   CALL contour(loads,g_coord,g_num,nci,argv,nlen,13)
 END IF

END SUBROUTINE p74
