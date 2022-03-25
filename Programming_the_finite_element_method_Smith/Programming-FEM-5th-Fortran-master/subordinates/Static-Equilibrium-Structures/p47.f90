SUBROUTINE p47(input_file,output_file)
!-------------------------------------------------------------------------
! Program 4.7 Analysis of plates using 4-node rectangular plate elements.
!             Homogeneous material with identical elements.
!             Mesh numbered in x- or y-direction.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,k,loaded_nodes,ndim=2,ndof=16,nels,neq,    &
   nip=16,nn,nod=4,nodof=4,nprops=2,np_types,nr,nxe,nye,nlen
 REAL(iwp)::aa,bb,d,d4=4.0_iwp,d12=12.0_iwp,e,one=1.0_iwp,                &
   penalty=1.0e20_iwp,th,two=2.0_iwp,v,zero=0.0_iwp
 CHARACTER(LEN=15)::element='quadrilateral',argv
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),kdiag(:),nf(:,:), &
   no(:),node(:),num(:),sense(:)
 REAL(iwp),ALLOCATABLE::bm(:),coord(:,:),dtd(:,:),d2x(:),d2xy(:),d2y(:),  &
   g_coord(:,:),km(:,:),kv(:),loads(:),points(:,:),prop(:,:),x_coords(:), &
   y_coords(:),value(:),weights(:)   
!-----------------------input and initialisation--------------------------
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,np_types,aa,bb,th 
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ALLOCATE(nf(nodof,nn),g_coord(ndim,nn),g_num(nod,nels),g(ndof),bm(3),    &
   g_g(ndof,nels),coord(nod,ndim),km(ndof,ndof),dtd(ndof,ndof),d2x(ndof), & 
   d2y(ndof),d2xy(ndof),num(nod),x_coords(nxe+1),y_coords(nye+1),         &
   points(nip,ndim),weights(nip),prop(nprops,np_types),etype(nels))
 READ(10,*)prop
 etype=1
 IF(np_types>1)read(10,*)etype
 DO i=1,nxe+1
   x_coords(i)=(i-1)*aa
 END DO
 DO i=1,nye+1
   y_coords(i)=(i-1)*bb
 END DO
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr) 
 CALL formnf(nf)  
 neq=MAXVAL(nf)
 ALLOCATE(kdiag(neq),loads(0:neq))
!-----------------------loop the elements to find global array sizes------
 kdiag=0
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,'x')
   CALL num_to_g(num,nf,g)
   CALL fkdiag(kdiag,g)
   g_num(:,iel)=num
   g_coord(:,num)=TRANSPOSE(coord)
   g_g(:,iel)=g
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
 elements_2: DO iel=1,nels
   e=prop(1,etype(iel))
   v=prop(2,etype(iel))
   d=e*th**3/(d12*(one-v**2))
   g=g_g(:,iel)
   km=zero
   integration: DO i=1,nip
     CALL fmplat(d2x,d2y,d2xy,points,aa,bb,i)
     DO k=1,ndof
         dtd(k,:)=d4*aa*bb*d*weights(i)*                                  &
         (d2x(k)*d2x(:)/(aa**4)+d2y(k)*d2y(:)/(bb**4)+(v*d2x(k)*d2y(:)+   &
         v*d2x(:)*d2y(k)+two*(one-v)*d2xy(k)*d2xy(:))/(aa**2*bb**2))
         dtd(:,k)=dtd(k,:)   
     END DO
     km=km+dtd
   END DO integration
   CALL fsparv(kv,km,g,kdiag)
 END DO elements_2
!-----------------------read loads and/or displacements-------------------
 loads=zero
 READ(10,*)loaded_nodes,(k,loads(nf(:,k)),i=1,loaded_nodes)
 READ(10,*)fixed_freedoms
 IF(fixed_freedoms/=0)then
   ALLOCATE(node(fixed_freedoms),no(fixed_freedoms),sense(fixed_freedoms),         &
     value(fixed_freedoms))
   READ(10,*)(node(i),sense(i),value(i),i=1,fixed_freedoms)
   DO i=1,fixed_freedoms
     no(i)=nf(sense(i),node(i))
   END DO
   kv(kdiag(no))=kv(kdiag(no))+penalty
   loads(no)=kv(kdiag(no))*value
 END IF
 CALL sparin(kv,kdiag)
 CALL spabac(kv,loads,kdiag)
 loads(0)=zero
 WRITE(11,'(/A)')"  Node   Disp        Rot-x       Rot-y       Twist-xy"
 DO k=1,nn
   WRITE(11,'(I5,6E12.4)')k,loads(nf(:,k))
 END DO
!-----------------------recover moments at element centroids--------------
 nip=1 
 DEALLOCATE(points)
 ALLOCATE(points(nip,ndim))
 CALL sample(element,points,weights)
 WRITE(11,'(/A)')(" Element x-moment    y-moment    xy-moment")
 DO iel=1,nels
   g=g_g(:,iel)
   moms: DO i=1,nip
     CALL fmplat(d2x,d2y,d2xy,points,aa,bb,i)
     bm=zero
     DO k=1,ndof
       bm(1)=bm(1)+d4*d*(d2x(k)/aa/aa+v*d2y(k)/bb/bb)*loads(g(k))
       bm(2)=bm(2)+d4*d*(v*d2x(k)/aa/aa+d2y(k)/bb/bb)*loads(g(k))
       bm(3)=bm(3)+d4*d*(one-v)*d2xy(k)/aa/bb*loads(g(k))
     END DO
   END DO moms
   WRITE(11,'(I5,3E12.4)')iel,bm
 END DO
END SUBROUTINE p47
