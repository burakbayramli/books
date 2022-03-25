SUBROUTINE p53(input_file,output_file)
!-------------------------------------------------------------------------
! Program 5.3 Three-dimensional analysis of an elastic solid using
!             8-, 14- or 20-node brick hexahedra. Mesh numbered in x-y
!             planes then in the z-direction.
!-------------------------------------------------------------------------
 USE main 
 USE geom 
 IMPLICIT NONE
 CHARACTER(len=70),INTENT(IN) :: input_file
 CHARACTER(len=70),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,k,loaded_nodes,ndim=3,ndof,nels,neq,nip,   &
   nlen,nn,nprops=2,np_types,nod,nodof=3,nr,nst=6,nxe,nye,nze   
 REAL(iwp)::det,penalty=1.0e20_iwp,zero=0.0_iwp    
 CHARACTER(LEN=15)::argv,element='hexahedron'
!-----------------------dynamic arrays-----------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),kdiag(:),nf(:,:), &
   no(:),node(:),num(:),sense(:)
 REAL(iwp),ALLOCATABLE::bee(:,:),coord(:,:),dee(:,:),der(:,:),deriv(:,:), &
   eld(:),fun(:),gc(:),g_coord(:,:),jac(:,:),km(:,:),kv(:),loads(:),      &
   points(:,:),prop(:,:),sigma(:),value(:),weights(:),x_coords(:),        &
   y_coords(:),z_coords(:)  
!-----------------------input and initialisation--------------------------

 OPEN(10,FILE=input_file) 
 OPEN(11,FILE=output_file)
 READ(10,*)nod,nxe,nye,nze,nip,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye,nze) 
 ndof=nod*nodof
 ALLOCATE(nf(nodof,nn),points(nip,ndim),dee(nst,nst),coord(nod,ndim),     &
   jac(ndim,ndim),der(ndim,nod),deriv(ndim,nod),g(ndof),bee(nst,ndof),    &
   km(ndof,ndof),eld(ndof),sigma(nst),g_g(ndof,nels),g_coord(ndim,nn),    &
   g_num(nod,nels),weights(nip),num(nod),prop(nprops,np_types),           &
   x_coords(nxe+1),y_coords(nye+1),z_coords(nze+1),etype(nels),fun(nod),  &
   gc(ndim))
 READ(10,*)prop 
 etype=1 
 IF(np_types>1)READ(10,*)etype
 READ(10,*)x_coords,y_coords,z_coords
 nf=1 
 READ(10,*)nr,(k,nf(:,k),i=1,nr) 
 CALL formnf(nf) 
 neq=MAXVAL(nf)
 ALLOCATE(loads(0:neq),kdiag(neq)) 
 kdiag=0
!-----------------------loop the elements to find global arrays sizes-----
 elements_1: DO iel=1,nels
   CALL hexahedron_xz(iel,x_coords,y_coords,z_coords,coord,num)
   CALL num_to_g(num,nf,g) 
   g_num(:,iel)=num
   g_coord(:,num)=TRANSPOSE(coord) 
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
 elements_2: DO iel=1,nels
   CALL deemat(dee,prop(1,etype(iel)),prop(2,etype(iel)))
   num=g_num(:,iel) 
   g=g_g(:,iel) 
   coord=TRANSPOSE(g_coord(:,num)) 
   km=zero
   gauss_pts_1: DO i=1,nip
     CALL shape_der(der,points,i) 
     jac=MATMUL(der,coord) 
     det=determinant(jac) 
     CALL invert(jac) 
     deriv=MATMUL(jac,der)
     CALL beemat(bee,deriv)
     km=km+MATMUL(MATMUL(TRANSPOSE(bee),dee),bee)*det*weights(i)
   END DO gauss_pts_1
   CALL fsparv(kv,km,g,kdiag)
 END DO elements_2                                             
 loads=zero 
 READ(10,*)loaded_nodes
 IF(loaded_nodes/=0)READ(10,*)(k,loads(nf(:,k)),i=1,loaded_nodes)
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
 WRITE(11,'(/A)')"  Node   x-disp      y-disp      z-disp"
 DO k=1,nn 
   WRITE(11,'(I5,3E12.4)')k,loads(nf(:,k)) 
 END DO
!-----------------------recover stresses at nip integrating points--------
 nip=1 
 DEALLOCATE(points,weights) 
 ALLOCATE(points(nip,ndim),weights(nip))
 CALL sample(element,points,weights)
 WRITE(11,'(/A,I2,A)')" The integration point (nip=",nip,") stresses are:"
 WRITE(11,'(A,/,A)')"    Element     x-coord     y-coord     z-coord",    &
   "    sig_x       sig_y       sig_z       tau_xy      tau_yz      tau_zx" 
 elements_3: DO iel=1,nels
   CALL deemat(dee,prop(1,etype(iel)),prop(2,etype(iel))) 
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num)) 
   g=g_g(:,iel) 
   eld=loads(g)
   gauss_pts_2: DO i=1,nip
     CALL shape_der(der,points,i) 
     CALL shape_fun(fun,points,i)
     gc=MATMUL(fun,coord) 
     jac=MATMUL(der,coord) 
     CALL invert(jac)
     deriv=MATMUL(jac,der) 
     CALL beemat(bee,deriv)
     sigma=MATMUL(dee,MATMUL(bee,eld)) 
     WRITE(11,'(I8,4X,3E12.4)')iel,gc
     WRITE(11,'(6E12.4)')sigma
   END DO gauss_pts_2
 END DO elements_3

END SUBROUTINE p53
