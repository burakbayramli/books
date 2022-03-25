SUBROUTINE p51(input_file,output_file)
!-------------------------------------------------------------------------
! Program 5.1 Plane or axisymmetric strain analysis of an elastic solid
!             using 3-, 6-, 10- or 15-node right-angled triangles or
!             4-, 8- or 9-node rectangular quadrilaterals. Mesh numbered
!             in x(r)- or y(z)- direction.
!-------------------------------------------------------------------------
 USE main 
 USE geom 
 IMPLICIT NONE
 CHARACTER(len=70),INTENT(IN) :: input_file
 CHARACTER(len=70),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,k,loaded_nodes,ndim=2,ndof,nels,neq,nip,   &
   nlen,nn,nod,nodof=2,nprops=2,np_types,nr,nst=3,nxe,nye 
 REAL(iwp)::det,one=1.0_iwp,penalty=1.0e20_iwp,zero=0.0_iwp
 CHARACTER(LEN=15)::argv,element,dir,type_2d
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),kdiag(:),nf(:,:), &
   no(:),node(:),num(:),sense(:)    
 REAL(iwp),ALLOCATABLE::bee(:,:),coord(:,:),dee(:,:),der(:,:),deriv(:,:), &
   eld(:),fun(:),gc(:),g_coord(:,:),jac(:,:),km(:,:),kv(:),loads(:),      &
   points(:,:),prop(:,:),sigma(:),value(:),weights(:),x_coords(:),        &
   y_coords(:)  
!-----------------------input and initialisation--------------------------

 OPEN(10,FILE=input_file) 
 OPEN(11,FILE=output_file)
 READ(10,*)type_2d,element,nod,dir,nxe,nye,nip,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 !ndof: number of degrees of freedom per element
 !nodof: number of degrees of freedom per node
 ndof=nod*nodof   
 IF(type_2d=='axisymmetric')nst=4
 ALLOCATE(nf(nodof,nn),points(nip,ndim),g(ndof),g_coord(ndim,nn),fun(nod),&
   coord(nod,ndim),jac(ndim,ndim),g_num(nod,nels),der(ndim,nod),          &
   deriv(ndim,nod),bee(nst,ndof),km(ndof,ndof),eld(ndof),weights(nip),    &
   g_g(ndof,nels),prop(nprops,np_types),num(nod),x_coords(nxe+1),         &
   y_coords(nye+1),etype(nels),gc(ndim),dee(nst,nst),sigma(nst))
 READ(10,*)prop 
 etype=1 
 IF(np_types>1)read(10,*)etype 
 READ(10,*)x_coords,y_coords
 nf=1 
 READ(10,*)nr,(k,nf(:,k),i=1,nr) 
 CALL formnf(nf) 
 neq=MAXVAL(nf)
 ALLOCATE(loads(0:neq),kdiag(neq)) 
 kdiag=0
!-----------------------loop the elements to find global arrays sizes-----
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,dir)
   CALL num_to_g(num,nf,g) 
   g_num(:,iel)=num
   g_coord(:,num)=TRANSPOSE(coord) 
   g_g(:,iel)=g 
   CALL fkdiag(kdiag,g)
 END DO elements_1
 CALL mesh(g_coord,g_num,argv,nlen,12)
 DO i=2,neq 
   kdiag(i)=kdiag(i)+kdiag(i-1) 
 END DO 
 ALLOCATE(kv(kdiag(neq)))
 WRITE(11,'(2(A,I5))')                                                    &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
!-----------------------element stiffness integration and assembly--------
 CALL sample(element,points,weights) 
 kv=zero 
 gc=one
 elements_2: DO iel=1,nels
   CALL deemat(dee,prop(1,etype(iel)),prop(2,etype(iel))) 
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num)) 
   g=g_g(:,iel) 
   km=zero
   int_pts_1: DO i=1,nip
     CALL shape_fun(fun,points,i) 
     CALL shape_der(der,points,i)
     jac=MATMUL(der,coord) 
     det=determinant(jac) 
     CALL invert(jac)
     deriv=MATMUL(jac,der) 
     CALL beemat(bee,deriv)
     IF(type_2d=='axisymmetric')THEN
       gc=MATMUL(fun,coord) 
       bee(4,1:ndof-1:2)=fun(:)/gc(1)
     END IF
     km=km+MATMUL(MATMUL(TRANSPOSE(bee),dee),bee)*det*weights(i)*gc(1)
   END DO int_pts_1
   CALL fsparv(kv,km,g,kdiag)
 END DO elements_2   
 loads=zero 
 READ(10,*)loaded_nodes,(k,loads(nf(:,k)),i=1,loaded_nodes)
 READ(10,*)fixed_freedoms
 IF(fixed_freedoms/=0)THEN
   ALLOCATE(node(fixed_freedoms),sense(fixed_freedoms),                   &
     value(fixed_freedoms),no(fixed_freedoms))
   READ(10,*)(node(i),sense(i),value(i),i=1,fixed_freedoms)
   DO i=1,fixed_freedoms 
     no(i)=nf(sense(i),node(i)) 
   END DO
   kv(kdiag(no))=kv(kdiag(no))+penalty 
   loads(no)=kv(kdiag(no))*value
 END IF
!-----------------------equation solution---------------------------------
 CALL sparin(kv,kdiag) 
 CALL spabac(kv,loads,kdiag) 
 loads(0)=zero
 IF(type_2d=='axisymmetric')THEN
   WRITE(11,'(/A)')"  Node   r-disp      z-disp" 
 ELSE
   WRITE(11,'(/A)')"  Node   x-disp      y-disp"
 END IF
 DO k=1,nn 
   WRITE(11,'(I5,2E12.4)')k,loads(nf(:,k)) 
 END DO
!-----------------------recover stresses at nip integrating points--------
 nip=1 
 DEALLOCATE(points,weights) 
 ALLOCATE(points(nip,ndim),weights(nip))
 CALL sample(element,points,weights)
 WRITE(11,'(/A,I2,A)')" The integration point (nip=",nip,") stresses are:"
 IF(type_2d=='axisymmetric')THEN
   WRITE(11,'(A,A)')" Element r-coord     z-coord",                       &
   "     sig_r       sig_z       tau_rz      sig_t" 
 ELSE 
   WRITE(11,'(A,A)')" Element x-coord     y-coord",                       &
   "     sig_x       sig_y       tau_xy" 
 END IF
 elements_3: DO iel=1,nels
   CALL deemat(dee,prop(1,etype(iel)),prop(2,etype(iel))) 
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num)) 
   g=g_g(:,iel) 
   eld=loads(g)
   int_pts_2: DO i=1,nip
     CALL shape_fun(fun,points,i) 
     CALL shape_der(der,points,i)
     gc=MATMUL(fun,coord) 
     jac=MATMUL(der,coord) 
     CALL invert(jac)
     deriv=MATMUL(jac,der) 
     CALL beemat(bee,deriv)
     IF(type_2d=='axisymmetric')THEN
       gc=MATMUL(fun,coord) 
       bee(4,1:ndof-1:2)=fun(:)/gc(1)
     END IF
     sigma=MATMUL(dee,MATMUL(bee,eld)) 
     WRITE(11,'(I5,6E12.4)')iel,gc,sigma
   END DO int_pts_2
 END DO elements_3
 CALL dismsh(loads,nf,0.05_iwp,g_coord,g_num,argv,nlen,13)
 CALL vecmsh(loads,nf,0.05_iwp,0.1_iwp,g_coord,g_num,argv,nlen,14)

END SUBROUTINE p51
