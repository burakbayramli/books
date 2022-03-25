SUBROUTINE p810(input_file,output_file)
!-------------------------------------------------------------------------
! Program 8.10 Plane analysis of the diffusion-convection equation
!              using 4-node rectangular quadrilaterals. Implicit time
!              integration using the "theta" method.
!              Untransformed solution.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,iel,j,k,nband,ndim=2,nels,neq,fixed_freedoms,nip=4,nlen,nn,   &
   nod=4,npri,np_types,nres,nstep,ntime,nxe,nye
 REAL(iwp)::det,dtim,part1,part2,pt2=0.2_iwp,penalty=1.0e20_iwp,theta,    &
   time,ux,uy,zero=0.0_iwp
 CHARACTER(LEN=15)::argv,element='quadrilateral'
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g_num(:,:),node(:),num(:)
 REAL(iwp),ALLOCATABLE::ans(:),conc(:),coord(:,:),copy(:,:),der(:,:),     &
   deriv(:,:),dtkd(:,:),fun(:),g_coord(:,:),jac(:,:),kb(:,:),kc(:,:),     &
   loads(:),ntn(:,:),pb(:,:),mm(:,:),points(:,:),prop(:,:),storpb(:),     &
   weights(:),work(:,:),x_coords(:),y_coords(:)
!-------------------------input and initialisation------------------------
 
 OPEN(10,FILE=input_file) 
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,np_types                       
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 neq=nn
 ALLOCATE(points(nip,ndim),weights(nip),coord(nod,ndim),fun(nod),         &
   jac(ndim,ndim),g_coord(ndim,nn),der(ndim,nod),deriv(ndim,nod),         &
   mm(nod,nod),g_num(nod,nels),kc(nod,nod),ntn(nod,nod),num(nod),         &
   dtkd(nod,nod),prop(ndim,np_types),x_coords(nxe+1),y_coords(nye+1),     &
   etype(nels),conc(nye+1))
 READ(10,*)prop
 etype=1
 if(np_types>1)read(10,*)etype
 READ(10,*)x_coords,y_coords
 READ(10,*)dtim,nstep,theta,npri,nres,ntime,ux,uy
 nband=0
!-----------------------loop the elements to find global arrays sizes-----
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,'x')
   g_num(:,iel)=num
   g_coord(:,num)=TRANSPOSE(coord)
   IF(nband<bandwidth(num))nband=bandwidth(num)
 END DO elements_1
 CALL mesh(g_coord,g_num,argv,nlen,12)
 ALLOCATE(kb(neq,2*nband+1),pb(neq,2*nband+1),work(nband+1,neq),          &
          copy(nband+1,neq),loads(0:neq),ans(0:neq))
 WRITE(11,'(2(a,i5))')                                                    &
 " There are",neq," equations and the half-bandwidth is",nband
 CALL sample(element,points,weights)
 kb=zero
 pb=zero
!-----------------------global conductivity and "mass" matrix assembly----
 elements_2: DO iel=1,nels
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
     DO j=1,nod
     DO k=1,nod
         part1=prop(1,etype(iel))*deriv(1,j)*deriv(1,k)+                  &
         prop(2,etype(iel))*deriv(2,j)*deriv(2,k)
         part2=ux*fun(j)*deriv(1,k)+uy*fun(j)*deriv(2,k)
         dtkd(j,k)=(part1-part2)*det*weights(i)
     END DO
     END DO
     kc=kc+dtkd
     CALL cross_product(fun,fun,ntn)
     mm=mm+ntn*det*weights(i)
   END DO gauss_pts
   mm=mm/(theta*dtim)
   CALL formtb(kb,kc,num)
   CALL formtb(pb,mm,num)
 END DO elements_2
 pb=pb+kb
 kb=pb-kb/theta
!-----------------------boundary conditions-------------------------------
 READ(10,*)fixed_freedoms 
 ALLOCATE(node(fixed_freedoms),storpb(fixed_freedoms))
 READ(10,*)node
 pb(node,nband+1)=pb(node,nband+1)+penalty
 storpb=pb(node,nband+1)
!-----------------------factorise equations-------------------------------
 work=zero
 CALL gauss_band(pb,work)
 WRITE(11,'(/a,i3,a)')"    Time      Concentration(node",nres,")"
 WRITE(11,'(2e12.4)')0.0,loads(nres)
 loads=zero
!-----------------------time stepping loop--------------------------------
 timesteps: DO j=1,nstep
   time=j*dtim
   copy=work
   CALL bantmul(kb,loads,ans)
   ans(0)=zero
   IF(time<=pt2)THEN
     ans(node)=storpb
   ELSE
     ans(node)=zero
   END IF
   CALL solve_band(pb,copy,ans)
   ans(0)=zero
   loads=ans
   IF(j==ntime)conc(:)=loads(2:neq:2)
   IF(j/npri*npri==j)WRITE(11,'(2e12.4)')time,loads(nres)
 END DO timesteps
 WRITE(11,'(/a,e10.4,a)')                                                 &
       "  Distance    Concentration(time=",nstep*dtim,")"
 DO i=1,nye+1
   WRITE(11,'(2e12.4)')y_coords(i),conc(i)
 END DO  

END SUBROUTINE p810
