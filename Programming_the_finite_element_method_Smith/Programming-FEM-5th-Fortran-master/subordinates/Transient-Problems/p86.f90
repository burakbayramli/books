SUBROUTINE p86(input_file,output_file)
!-------------------------------------------------------------------------
! Program 8.6 Plane or axisymmetric analysis of the consolidation equation
!             using 4-node rectangular quadrilaterals. Mesh numbered in
!             x(r)- or y(z)- direction. Explicit time integration using
!             the "theta=0" method.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,j,nci,ndim=2,nels,neq,nip=4,nlen,nn,nod=4, &
   npri,np_types,nres,nstep,ntime,nxe,nye
 REAL(iwp)::det,dtim,one=1.0_iwp,time,zero=0.0_iwp   
 CHARACTER(len=15)::argv,dir,element='quadrilateral',type_2d
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g_num(:,:),node(:),num(:)
 REAL(iwp),ALLOCATABLE::coord(:,:),der(:,:),deriv(:,:),fun(:),gc(:),      &
   globma(:),g_coord(:,:),jac(:,:),kay(:,:),kc(:,:),loads(:),mass(:),     &
   newlo(:),ntn(:,:),mm(:,:),points(:,:),prop(:,:),store_mm(:,:,:),       &
   value(:),weights(:),x_coords(:),y_coords(:)
!-----------------------input and initialisation--------------------------
 
 OPEN(10,FILE=input_file) 
 OPEN(11,FILE=output_file)
 READ(10,*)type_2d,dir,nxe,nye,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 neq=nn
 WRITE(11,'(a,i5,a)')" There are",neq," equations"
 ALLOCATE(points(nip,ndim),weights(nip),kay(ndim,ndim),globma(0:neq),     &
   coord(nod,ndim),fun(nod),jac(ndim,ndim),g_coord(ndim,nn),der(ndim,nod),&
   deriv(ndim,nod),mm(nod,nod),g_num(nod,nels),kc(nod,nod),ntn(nod,nod),  &
   num(nod),etype(nels),x_coords(nxe+1),y_coords(nye+1),                  &
   prop(ndim,np_types),gc(ndim),store_mm(nod,nod,nels),mass(nod),         &
   loads(0:neq),newlo(0:neq))
 READ(10,*)prop
 etype=1
 IF(np_types>1)read(10,*)etype
 READ(10,*)x_coords,y_coords
 READ(10,*)dtim,nstep,npri,nres,ntime
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
     IF(type_2d=='axisymmetric')gc=MATMUL(fun,coord)
     kc=kc+MATMUL(MATMUL(TRANSPOSE(deriv),kay),deriv)*det*weights(i)*gc(1)
     CALL cross_product(fun,fun,ntn)
     mm=mm+ntn*det*weights(i)*gc(1)
   END DO gauss_pts
   DO i=1,nod
     mass(i)=SUM(mm(i,:))
   END DO
   mm=zero
   DO i=1,nod
     mm(i,i)=mass(i)
   END DO
   store_mm(:,:,iel)=mm-kc*dtim
   globma(num)=globma(num)+mass
 END DO elements_1
 globma(1:)=one/globma(1:)
 CALL mesh(g_coord,g_num,argv,nlen,12)
!-----------------------specify initial and boundary values---------------
 READ(10,*)loads(1:)
 loads(0)=zero
 READ(10,*)fixed_freedoms
 IF(fixed_freedoms/=0)then
   ALLOCATE(node(fixed_freedoms),value(fixed_freedoms))
   READ(10,*)(node(i),value(i),i=1,fixed_freedoms)
 END IF
!-----------------------time stepping loop--------------------------------
 WRITE(11,'(/a,i3,a)')"    Time      Pressure (node",nres,")"
 WRITE(11,'(2e12.4)')0.0,loads(nres)
   timesteps: DO j=1,nstep
   time=j*dtim
   newlo=zero
   elements_2: DO iel=1,nels  
     num=g_num(:,iel)
     mm=store_mm(:,:,iel) 
     newlo(num)=newlo(num)+MATMUL(mm,loads(num))
   END DO elements_2
   newlo(0)=zero
   loads=newlo*globma
   IF(fixed_freedoms/=0)loads(node)=value
   IF(nod==4.AND.j==ntime)THEN
     READ(10,*)nci
     CALL contour(loads,g_coord,g_num,nci,argv,nlen,13)
   END IF
   IF(j/npri*npri==j)WRITE(11,'(2e12.4)')time,loads(nres)
 END DO timesteps

END SUBROUTINE p86
