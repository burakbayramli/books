SUBROUTINE p89(input_file,output_file)     
!-------------------------------------------------------------------------
! Program 8.9 Plane analysis of the diffusion-convection equation
!             using 4-node rectangular quadrilaterals. Implicit time
!             integration using the "theta" method.
!             Self-adjoint transformation.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file                
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,iel,j,nci,ndim=2,nels,neq,nip=4,nlen,nn,nod=4,npri,np_types,  &
 nres,nstep,ntime,nxe,nye
 REAL(iwp)::det,dtim,d6=6.0_iwp,d12=12.0_iwp,f1,f2,pt25=0.25_iwp,theta,   &
   time,two=2.0_iwp,ux,uy,zero=0.0_iwp 
 CHARACTER(LEN=15)::argv,element='quadrilateral'
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g_num(:,:),num(:),kdiag(:)
 REAL(iwp),ALLOCATABLE::ans(:),bp(:),coord(:,:),der(:,:),deriv(:,:),      &
   fun(:),g_coord(:,:),jac(:,:),kay(:,:),kc(:,:),kv(:),loads(:),ntn(:,:), &
   mm(:,:),points(:,:),prop(:,:),weights(:),x_coords(:),y_coords(:)
!-------------------------input and initialisation------------------------
 
 OPEN(10,FILE=input_file) 
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,np_types 
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 neq=nn
 ALLOCATE(points(nip,ndim),weights(nip),kay(ndim,ndim),coord(nod,ndim),   &
   fun(nod),jac(ndim,ndim),g_coord(ndim,nn),der(ndim,nod),deriv(ndim,nod),&
   mm(nod,nod),g_num(nod,nels),kc(nod,nod),ntn(nod,nod),num(nod),         &
   prop(ndim,np_types),x_coords(nxe+1),y_coords(nye+1),etype(nels),       &
   kdiag(neq),loads(0:neq),ans(0:neq))
 READ(10,*)prop
 etype=1
 if(np_types>1)read(10,*)etype
 READ(10,*)x_coords,y_coords 
 READ(10,*)dtim,nstep,theta,npri,nres,ntime,ux,uy,nci
 kdiag=0
!-----------------------loop the elements to find global arrays sizes-----
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,'x')
   g_num(:,iel)=num
   g_coord(:,num)=TRANSPOSE(coord)
   CALL fkdiag(kdiag,num)
 END DO elements_1
 CALL mesh(g_coord,g_num,argv,nlen,12) 
 DO i=2,neq
   kdiag(i)=kdiag(i)+kdiag(i-1)
 END DO 
 ALLOCATE(kv(kdiag(neq)),bp(kdiag(neq)))
 WRITE(11,'(2(a,i5),/)')                                                  &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
 CALL sample(element,points,weights)
 kv=zero
 bp=zero
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
     kc=kc+MATMUL(MATMUL(TRANSPOSE(deriv),kay),deriv)*det*weights(i)
     CALL cross_product(fun,fun,ntn)
     mm=mm+ntn*det*weights(i)
   END DO gauss_pts
   kc=kc+mm*(ux*ux/kay(1,1)+uy*uy/kay(2,2))*pt25
   mm=mm/(theta*dtim)
!-----------------------derivative boundary conditions--------------------
   IF(iel==1)THEN
     det=x_coords(2)-x_coords(1)
     kc(2,2)=kc(2,2)+uy*det/d6
     kc(2,3)=kc(2,3)+uy*det/d12
     kc(3,2)=kc(3,2)+uy*det/d12
     kc(3,3)=kc(3,3)+uy*det/d6
   ELSE IF(iel==nels)THEN
     det=x_coords(2)-x_coords(1)
     kc(1,1)=kc(1,1)+uy*det/d6
     kc(1,4)=kc(1,4)+uy*det/d12
     kc(4,1)=kc(4,1)+uy*det/d12
     kc(4,4)=kc(4,4)+uy*det/d6
   END IF
   CALL fsparv(kv,kc,num,kdiag)
   CALL fsparv(bp,mm,num,kdiag)
 END DO elements_2
 f1=uy*det/(two*theta)
 f2=f1
 bp=bp+kv
 kv=bp-kv/theta 
!-----------------------factorise equations-------------------------------
 CALL sparin(bp,kdiag)
 loads=zero                    
!-----------------------time stepping loop--------------------------------
 WRITE(11,'(a,i3,a)')"    Time     Concentration (node",nres,")"
 WRITE(11,'(2e12.4)')0.0,loads(nres)
 timesteps: DO j=1,nstep
   time=j*dtim
   CALL linmul_sky(kv,loads,ans,kdiag)
   ans(neq)=ans(neq)+f1
   ans(neq-1)=ans(neq-1)+f2
   CALL spabac(bp,ans,kdiag)
   loads=ans
   IF(nod==4.AND.j==ntime)CALL contour(loads,g_coord,g_num,nci,argv,nlen,13)
   IF(j/npri*npri==j)WRITE(11,'(2e12.4)')                                 &
     time,loads(nres)*exp(-ux*g_coord(1,nres)/two/kay(1,1))*              &
     exp(-uy*g_coord(2,nres)/two/kay(2,2))
 END DO timesteps

END SUBROUTINE p89
