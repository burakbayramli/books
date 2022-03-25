SUBROUTINE p87(input_file,output_file)
!-------------------------------------------------------------------------
! Program 8.7 Plane or axisymmetric analysis of the consolidation equation
!             using 4-node rectangular quadrilaterals. Mesh numbered in
!             x(r)- or y(z)- direction. "theta" method using an
!             "element-by-element" (ebe) product algorithm.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,j,nci,ndim=2,nels,neq,nip=4,nlen,nn,nod=4, &
   npri,np_types,nres,nstep,ntime,nxe,nye
 REAL(iwp)::det,dtim,one=1.0_iwp,pt5=0.5_iwp,theta,time,zero=0.0_iwp
 CHARACTER(LEN=15)::argv,dir,element='quadrilateral',type_2d
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g_num(:,:),node(:),num(:)
 REAL(iwp),ALLOCATABLE::coord(:,:),der(:,:),deriv(:,:),fun(:),gc(:),      &
   globma(:),g_coord(:,:),jac(:,:),kay(:,:),kc(:,:),loads(:),ntn(:,:),    &
   mm(:,:),points(:,:),prop(:,:),store_kc(:,:,:),value(:),weights(:),     &
   x_coords(:),y_coords(:)
!-----------------------input and initialisation--------------------------
 
 OPEN(10,FILE=input_file) 
 OPEN(11,FILE=output_file)
 READ(10,*)type_2d,dir,nxe,nye,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 neq=nn
 WRITE(11,'(a,i5,a)')" There are",neq," equations"
 ALLOCATE(points(nip,ndim),weights(nip),kay(ndim,ndim),coord(nod,ndim),   &
   fun(nod),jac(ndim,ndim),g_coord(ndim,nn),der(ndim,nod),deriv(ndim,nod),&
   mm(nod,nod),g_num(nod,nels),kc(nod,nod),ntn(nod,nod),num(nod),         &
   globma(0:nn),store_kc(nod,nod,nels),gc(ndim),loads(0:neq),             &
   x_coords(nxe+1),y_coords(nye+1),prop(ndim,np_types),etype(nels))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)x_coords,y_coords
 READ(10,*)dtim,nstep,theta,npri,nres,ntime
 CALL sample(element,points,weights)
 globma=zero
 gc=one
 !---------create and store element and global lumped mass matrices--------
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,dir)
   kay=zero
   DO i=1,ndim
     kay(i,i)=prop(i,etype(iel))
   END DO
   g_num(:,iel)=num
   g_coord(:,num)=TRANSPOSE(coord)
   kc=zero
   mm=zero
   gauss_pts: DO i=1,nip
     CALL shape_der(der,points,i)
     CALL shape_fun(fun,points,i)
     jac=MATMUL(der,coord)
     det=determinant(jac)
     CALL invert(jac)
     deriv=MATMUL(jac,der)
     if(type_2d=='axisymmetric')gc=MATMUL(fun,coord)
     kc=kc+MATMUL(MATMUL(TRANSPOSE(deriv),kay),deriv)*det*weights(i)*gc(1)
     CALL cross_product(fun,fun,ntn)
     mm=mm+ntn*det*weights(i)*gc(1)
   END DO gauss_pts
   store_kc(:,:,iel)=kc
   DO i=1,nod
     globma(num(i))=globma(num(i))+SUM(mm(i,:))
   END DO
 END DO elements_1
 CALL mesh(g_coord,g_num,argv,nlen,12)
!-----------------------recover element a and b matrices------------------
 elements_2: DO iel=1,nels
   num=g_num(:,iel)
   kc=-store_kc(:,:,iel)*(one-theta)*dtim*pt5
   mm=store_kc(:,:,iel)*theta*dtim*pt5
   DO i=1,nod
     mm(i,i)=mm(i,i)+globma(num(i))
     kc(i,i)=kc(i,i)+globma(num(i))
   END DO
   CALL invert(mm)
   mm=MATMUL(mm,kc)
   store_kc(:,:,iel)=mm
 END DO elements_2
!-----------------------specify initial and boundary values---------------
 READ(10,*)loads(1:)
 loads(0)=zero
 READ(10,*)fixed_freedoms
 IF(fixed_freedoms/=0)THEN
   ALLOCATE(node(fixed_freedoms),value(fixed_freedoms))
   READ(10,*)(node(i),value(i),i=1,fixed_freedoms)
 END IF
!-----------------------time stepping loop--------------------------------
 WRITE(11,'(/a,i3,a)')"    Time      Pressure (node",nres,")"
 WRITE(11,'(2e12.4)')0.0,loads(nres)
 timesteps: DO j=1,nstep
   time=j*dtim
!-----------------------first pass (1 to nels)----------------------------
   elements_3: DO iel=1,nels
     num=g_num(:,iel)
     mm=store_kc(:,:,iel)
     loads(num)=MATMUL(mm,loads(num))
     loads(0)=zero
     loads(node)=value
   END DO elements_3
!-----------------------second pass (nels to 1)---------------------------
   elements_4: DO iel=nels,1,-1
     num=g_num(:,iel)
     mm=store_kc(:,:,iel)
     loads(num)=MATMUL(mm,loads(num))
     loads(0)=zero
     loads(node)=value
   END DO elements_4
   IF(nod==4.AND.j==ntime)THEN
     READ(10,*)nci
     CALL contour(loads,g_coord,g_num,nci,argv,nlen,13)
   END IF
   IF(j/npri*npri==j)WRITE(11,'(2e12.4)')time,loads(nres)
 END DO timesteps

END SUBROUTINE p87
