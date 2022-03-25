SUBROUTINE p811(input_file,output_file)
!-------------------------------------------------------------------------
! Program 8.11 Plane or axisymmetric thermal conduction analysis using
!         4-node rectangular quadrilaterals. Option of convection and
!         flux boundary conditions.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,j,k,nci,ncon,ndim=2,nels,neq,nflx,nip=4,   &
   nlen,nn,nod=4,nodl=2,npri,nprops=3,np_types,nres,nstep,ntime,nxe,nye
 REAL(iwp)::det,dtim,d3=3.0_iwp,d4=4.0_iwp,d6=6.0_iwp,d12=12.0_iwp,       &
   hpl,one=1.0_iwp,length,penalty=1.0e20_iwp,pt5=0.5_iwp,theta,time,      &
   two=2.0_iwp,zero=0.0_iwp
 CHARACTER(len=15)::argv,dir,element='quadrilateral',type_2d
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::econn(:,:),etype(:),g_num(:,:),node(:),num(:),kdiag(:)  
 REAL(iwp),ALLOCATABLE::bp(:),coord(:,:),der(:,:),deriv(:,:),econv(:,:),  &
   flxv(:),fun(:),gc(:),g_coord(:,:),jac(:,:),kay(:,:),kc(:,:),kcl(:,:),  &
   kv(:),loads(:),newlo(:),ntn(:,:),mm(:,:),points(:,:),prop(:,:),rhs(:), &
   storbp(:),value(:),weights(:),x_coords(:),y_coords(:)
!-------------------------input and initialisation------------------------
 
 OPEN(10,FILE=input_file) 
 OPEN(11,FILE=output_file)
 READ(10,*)type_2d,dir,nxe,nye,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 neq=nn
 ALLOCATE(points(nip,ndim),weights(nip),kay(ndim,ndim),coord(nod,ndim),   &
   fun(nod),jac(ndim,ndim),g_coord(ndim,nn),der(ndim,nod),deriv(ndim,nod),&
   mm(nod,nod),g_num(nod,nels),kc(nod,nod),ntn(nod,nod),num(nod),         &
   etype(nels),kdiag(neq),loads(0:neq),newlo(0:neq),flxv(0:neq),          &
   x_coords(nxe+1),y_coords(nye+1),prop(nprops,np_types),gc(ndim),        &
   kcl(nodl,nodl),rhs(0:neq))
 READ(10,*)prop
 etype=1
 if(np_types>1)read(10,*)etype
 READ(10,*)x_coords,y_coords
 READ(10,*)dtim,nstep,theta,npri,nres,ntime
 kdiag=0
! ----------loop the elements to set up global geometry and kdiag --------
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,dir)
   g_num(:,iel)=num
   g_coord(:,num)=TRANSPOSE(coord) 
   CALL fkdiag(kdiag,num)
 END DO elements_1
 CALL mesh(g_coord,g_num,argv,nlen,12)
 DO i=2,neq
   kdiag(i)=kdiag(i)+kdiag(i-1)
 END DO
 ALLOCATE(kv(kdiag(neq)),bp(kdiag(neq))) 
 WRITE(11,'(2(A,I5))')                                                    &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
 CALL sample(element,points,weights)
 bp=zero
 kv=zero
 gc=one
!-----------------------global conductivity and "mass" matrix assembly----
 elements_2: DO iel=1,nels
   kay=zero
   DO i=1,ndim
     kay(i,i)=prop(i,etype(iel))
   END DO
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
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
     mm=mm+ntn*det*weights(i)*gc(1)*prop(3,etype(iel))
   END DO gauss_pts
   CALL fsparv(kv,kc,num,kdiag)
   CALL fsparv(bp,mm,num,kdiag)
 END DO elements_2 
 flxv=zero
 READ(10,*)nflx,(k,flxv(k),i=1,nflx);rhs=flxv
 READ(10,*)ncon
 IF(ncon/=0)THEN
   ALLOCATE(econn(nodl,ncon),econv(2,ncon))
   READ(10,*)(econn(:,i),econv(:,i),i=1,ncon)
   DO i=1,ncon
     length=SQRT((g_coord(1,econn(1,i))-g_coord(1,econn(2,i)))**2         &
                +(g_coord(2,econn(1,i))-g_coord(2,econn(2,i)))**2)
     hpl=econv(1,i)*length
     IF(type_2d=='axisymmetric')THEN
       rhs(econn(1,i))=rhs(econn(1,i))+hpl*econv(2,i)*                    &
	      (two*g_coord(1,econn(1,i))+g_coord(1,econn(2,i)))/d6
       rhs(econn(2,i))=rhs(econn(2,i))+hpl*econv(2,i)*                    &
	      (two*g_coord(1,econn(2,i))+g_coord(1,econn(1,i)))/d6
       kcl(1,1)=hpl*(g_coord(1,econn(1,i))/d4+g_coord(1,econn(2,i))/d12)
       kcl(1,2)=hpl*(g_coord(1,econn(1,i))+g_coord(1,econn(2,i)))/d12
       kcl(2,2)=hpl*(g_coord(1,econn(2,i))/d4+g_coord(1,econn(1,i))/d12)
       kcl(2,1)=kcl(1,2)
     ELSE
       rhs(econn(:,i))=rhs(econn(:,i))+hpl*econv(2,i)*pt5
       kcl(1,1)=hpl/d3
       kcl(1,2)=hpl/d6
       kcl(2,1)=kcl(1,2)
       kcl(2,2)=kcl(1,1)
	 ENDIF
     CALL fsparv(kv,kcl,econn(:,i),kdiag)
   END DO
 END IF
 kv=kv*theta*dtim
 bp=bp+kv
 kv=bp-kv/theta
 rhs=rhs*dtim
!-----------------------specify initial and boundary values---------------
 READ(10,*)loads(1:)
 loads(0)=zero
 READ(10,*)fixed_freedoms
 IF(fixed_freedoms/=0)then
   ALLOCATE(node(fixed_freedoms),value(fixed_freedoms),                   &
            storbp(fixed_freedoms))
   READ(10,*)(node(i),value(i),i=1,fixed_freedoms)
   bp(kdiag(node))=bp(kdiag(node))+penalty
   storbp=bp(kdiag(node))
 END IF
!-----------------------factorise equations-------------------------------
 CALL sparin(bp,kdiag)                    
!-----------------------time stepping loop--------------------------------
 WRITE(11,'(/A,I3,A)')"    Time      Pressure (node",nres,")"
 WRITE(11,'(2E12.4)')0.0,loads(nres)
 timesteps: DO j=1,nstep
   time=j*dtim
   CALL linmul_sky(kv,loads,newlo,kdiag)
   newlo=newlo+rhs
   IF(fixed_freedoms/=0)newlo(node)=storbp*value
   CALL spabac(bp,newlo,kdiag)
   loads=newlo  
   IF(nod==4.AND.j==ntime)THEN
     READ(10,*)nci
     CALL contour(loads,g_coord,g_num,nci,argv,nlen,13)
   END IF
   IF(j/npri*npri==j)WRITE(11,'(2E12.4)')time,loads(nres)
 END DO timesteps

END SUBROUTINE p811
