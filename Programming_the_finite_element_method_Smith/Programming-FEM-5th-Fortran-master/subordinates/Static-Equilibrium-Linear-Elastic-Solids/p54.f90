SUBROUTINE p54(input_file,output_file) 
!-------------------------------------------------------------------------
! Program 5.4 General two- (plane strain) or three-dimensional analysis
!             of elastic solids (optional gravity loading).
!-------------------------------------------------------------------------
 USE main 
 USE geom 
 IMPLICIT NONE
 CHARACTER(len=70),INTENT(IN) :: input_file
 CHARACTER(len=70),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,k,loaded_nodes,ndim,ndof,nels,neq,nip,nlen,&
   nn,nod,nodof,nprops=3,np_types,nr,nst 
 REAL(iwp)::det,penalty=1.0e20_iwp,zero=0.0_iwp
 CHARACTER(len=15)::argv,element
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),kdiag(:),nf(:,:), &
   no(:),node(:),num(:),sense(:)    
 REAL(iwp),ALLOCATABLE::bee(:,:),coord(:,:),dee(:,:),der(:,:),deriv(:,:), &
   eld(:),fun(:),gc(:),gravlo(:),g_coord(:,:),jac(:,:),km(:,:),kv(:),     &
   loads(:),points(:,:),prop(:,:),sigma(:),value(:),weights(:)  
!-----------------------input and initialisation--------------------------

 OPEN(10,FILE=input_file) 
 OPEN(11,FILE=output_file)
 READ(10,*)element,nod,nels,nn,nip,nodof,nst,ndim,np_types 
 ndof=nod*nodof
 ALLOCATE(nf(nodof,nn),points(nip,ndim),dee(nst,nst),g_coord(ndim,nn),    &
   coord(nod,ndim),jac(ndim,ndim),weights(nip),num(nod),g_num(nod,nels),  &
   der(ndim,nod),deriv(ndim,nod),bee(nst,ndof),km(ndof,ndof),eld(ndof),   &
   sigma(nst),g(ndof),g_g(ndof,nels),gc(ndim),fun(nod),etype(nels),       &
   prop(nprops,np_types))
 READ(10,*)prop 
 etype=1 
 IF(np_types>1)READ(10,*)etype
 READ(10,*)g_coord 
 READ(10,*)g_num
 IF(ndim==2)CALL mesh(g_coord,g_num,argv,nlen,12)
 nf=1 
 READ(10,*)nr,(k,nf(:,k),i=1,nr) 
 CALL formnf(nf) 
 neq=MAXVAL(nf) 
 ALLOCATE(kdiag(neq),loads(0:neq),gravlo(0:neq)) 
 kdiag=0
!-----------------------loop the elements to find global arrays sizes-----
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
!-----------------------element stiffness integration and assembly--------
 CALL sample(element,points,weights) 
 kv=zero 
 gravlo=zero
 elements_2: DO  iel=1,nels
   CALL deemat(dee,prop(1,etype(iel)),prop(2,etype(iel))) 
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num)) 
   g=g_g(:,iel) 
   km=zero 
   eld=zero
   int_pts_1: DO i=1,nip
     CALL shape_fun(fun,points,i) 
     CALL shape_der(der,points,i)
     jac=MATMUL(der,coord) 
     det=determinant(jac) 
     CALL invert(jac)
     deriv=MATMUL(jac,der) 
     CALL beemat(bee,deriv)
     km=km+MATMUL(MATMUL(transpose(bee),dee),bee)*det*weights(i)
     eld(nodof:ndof:nodof)=eld(nodof:ndof:nodof)+fun(:)*det*weights(i)
   END DO int_pts_1
   CALL fsparv(kv,km,g,kdiag) 
   gravlo(g)=gravlo(g)-eld*prop(3,etype(iel))
 END DO elements_2
 loads=zero 
 READ(10,*)loaded_nodes,(k,loads(nf(:,k)),i=1,loaded_nodes)
 loads=loads+gravlo 
 READ(10,*)fixed_freedoms
 IF(fixed_freedoms/=0)THEN
   ALLOCATE(node(fixed_freedoms),sense(fixed_freedoms),                   &
     value(fixed_freedoms),no(fixed_freedoms))
   READ(10,*)(node(i),sense(i),value(i),i=1,fixed_freedoms)
   DO  i=1,fixed_freedoms 
     no(i)=nf(sense(i),node(i)) 
   END DO 
   kv(kdiag(no))=kv(kdiag(no))+penalty 
   loads(no)=kv(kdiag(no))*value
 END IF
!-----------------------equation solution---------------------------------
 CALL sparin(kv,kdiag) 
 CALL spabac(kv,loads,kdiag) 
 loads(0)=zero
 IF(ndim==3)THEN 
   WRITE(11,'(/A)')"  Node   x-disp      y-disp      z-disp"
 ELSE 
   WRITE(11,'(/A)')"  Node   x-disp      y-disp"
 END IF
 DO k=1,nn 
   WRITE(11,'(I5,3E12.4)')k,loads(nf(:,k)) 
 END DO
!-----------------------recover stresses at element Gauss-points----------
!nip=1 
!DEALLOCATE(points,weights) 
!ALLOCATE(points(nip,ndim),weights(nip))
!CALL sample(element,points,weights)
 WRITE(11,'(/A,I2,A)')" The integration point (nip=",nip,") stresses are:"
 IF(ndim==3)THEN        
   WRITE(11,'(A,/,A)')"    Element     x-coord     y-coord     z-coord",  &
   "    sig_x       sig_y       sig_z       tau_xy      tau_yz      tau_zx" 
 ELSE 
   WRITE(11,'(A,A)')  "    Element x-coord     y-coord",              &
   "          sig_x       sig_y       tau_xy" 
 END IF
 elements_3: DO iel=1,nels
   CALL deemat(dee,prop(1,etype(iel)),prop(2,etype(iel))) 
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num)) 
   g=g_g(:,iel) 
   eld=loads(g)
   int_pts_2: DO i=1,nip
     CALL shape_der(der,points,i) 
     CALL shape_fun(fun,points,i)
     gc=MATMUL(fun,coord) 
     jac=MATMUL(der,coord) 
     CALL invert(jac)
     deriv=MATMUL(jac,der) 
     CALL beemat(bee,deriv)
     sigma=MATMUL(dee,MATMUL(bee,eld))
     IF(ndim==3)THEN 
       WRITE(11,'(I8,4X,3E12.4)')iel,gc
       WRITE(11,'(6E12.4)')sigma
     ELSE 
       WRITE(11,'(I8,2E12.4,5X,3E12.4)')iel,gc,sigma
     END IF
   END DO int_pts_2
 END DO elements_3
 IF(ndim==2)THEN 
   CALL dismsh(loads,nf,0.05_iwp,g_coord,g_num,argv,nlen,13)
   CALL vecmsh(loads,nf,0.05_iwp,0.1_iwp,g_coord,g_num,argv,nlen,14)
 END IF

END SUBROUTINE p54
