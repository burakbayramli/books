SUBROUTINE p116(input_file,output_file)
!-------------------------------------------------------------------------
! Program 11.6 Forced vibration analysis of an elastic solid in plane
!              strain using rectangular uniform size 4-node quadrilaterals.
!              Mesh numbered in the x- or y-direction. Lumped and/or 
!              consistent mass. Mixed explicit/implicit time integration.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,j,iel,k,ndim=2,ndof=8,nels,neq,nip=4,nlen,nn,nod=4,nodof=2,   &
   npri,nprops=3,np_types,nr,nres,nstep,nxe,nye
 REAL(iwp)::area,beta,c1,det,dtim,gamma,one=1.0_iwp,pt5=0.5_iwp,time,     &
   two=2.0_iwp,zero=0.0_iwp
 CHARACTER(LEN=15)::argv,element='quadrilateral'
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),kdiag_l(:),       &
   kdiag_r(:),nf(:,:),num(:)   
 REAL(iwp),ALLOCATABLE::coord(:,:),der(:,:),d1x0(:),d1x1(:),d2x0(:),      &
   d2x1(:),ecm(:,:),fun(:),g_coord(:,:),jac(:,:),km(:,:),kv(:),mm(:,:),   &
   mv(:),points(:,:),prop(:,:),weights(:),x0(:),x1(:),x_coords(:),        &
   y_coords(:)
   CHARACTER(LEN=1),ALLOCATABLE::mtype(:)
!-----------------------input and initialisation--------------------------
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ALLOCATE(nf(nodof,nn),points(nip,ndim),g(ndof),g_coord(ndim,nn),         &
   coord(nod,ndim),jac(ndim,ndim),weights(nip),der(ndim,nod),             &
   km(ndof,ndof),num(nod),g_num(nod,nels),g_g(ndof,nels),mtype(nels),     &
   mm(ndof,ndof),ecm(ndof,ndof),fun(nod),prop(nprops,np_types),           &
   x_coords(nxe+1),y_coords(nye+1),etype(nels))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)x_coords,y_coords,dtim,nstep,beta,gamma,npri,nres
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)
 ALLOCATE(kdiag_l(neq),kdiag_r(neq),x0(0:neq),d1x0(0:neq),x1(0:neq),      &
   d2x0(0:neq),d1x1(0:neq),d2x1(0:neq))
 READ(10,*)mtype
 CALL sample(element,points,weights)
 kdiag_l=0
 kdiag_r=0
!-----------------------loop the elements to find global array sizes------
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,'y')
   CALL num_to_g(num,nf,g)
   g_num(:,iel)=num
   g_coord(:,num)=TRANSPOSE(coord)
   g_g(:,iel)=g
   CALL fkdiag(kdiag_r,g)
   IF(mtype(iel)=='c')CALL fkdiag(kdiag_l,g)
 END DO elements_1
 CALL mesh(g_coord,g_num,argv,nlen,12)
 WHERE(kdiag_l==0)
   kdiag_l=1
 END WHERE
 DO i=2,neq
   kdiag_l(i)=kdiag_l(i)+kdiag_l(i-1)   
   kdiag_r(i)=kdiag_r(i)+kdiag_r(i-1)
 END DO
 ALLOCATE(kv(kdiag_l(neq)),mv(kdiag_r(neq)))
 WRITE(11,'(A,I5,A,/,2(A,I5),A)')                                         &
   " There are",neq," equations."," Skyline storage is",kdiag_l(neq),     &
   " to the left, and",kdiag_r(neq)," to the right."
 c1=one/dtim/dtim/beta
 kv=zero
 mv=zero
!-----------------------global stiffness and mass matrix assembly---------
 elements_2: DO iel=1,nels
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   CALL stiff4(km,coord,prop(1,etype(iel)),prop(2,etype(iel)))
   g=g_g(:,iel)
   area=zero
   mm=zero
   gauss_pts_1: DO i=1,nip
     CALL shape_der(der,points,i)
     jac=MATMUL(der,coord)
     det=determinant(jac)
     area=area+det*weights(i)
     CALL shape_fun(fun,points,i)
     IF(mtype(iel)=='c')THEN
       CALL ecmat(ecm,fun,ndof,nodof)
       mm=mm+ecm*det*weights(i)*c1*prop(3,etype(iel))
     END IF
   END DO gauss_pts_1
   IF(mtype(iel)=='l')THEN
     CALL elmat(area,c1*prop(3,etype(iel)),mm)
     CALL fsparv(kv,mm,g,kdiag_l)
     CALL fsparv(mv,mm-km,g,kdiag_r)
   ELSE
     CALL fsparv(kv,km+mm,g,kdiag_l)
     CALL fsparv(mv,mm,g,kdiag_r)
   END IF
 END DO elements_2             
!-----------------------initial conditions and factorise equations--------
 x0=zero
 d1x0=one
 d2x0=zero
 CALL sparin(kv,kdiag_l)
 time=zero
!-----------------------time stepping loop--------------------------------
 WRITE(11,'(/A,I5)')" Result at node",nres
 WRITE(11,'(A)')"    time        x-disp      y-disp"
 WRITE(11,'(4E12.4)')time,x0(nf(:,nres))
 timesteps: DO j=1,nstep
   time=time+dtim
   d1x1=x0+d1x0*dtim+d2x0*pt5*dtim*dtim*(one-two*beta)
   CALL linmul_sky(mv,d1x1,x1,kdiag_r)
   CALL spabac(kv,x1,kdiag_l)
   d2x1=(x1-d1x1)/dtim/dtim/beta 
   d1x1=d1x0+d2x0*dtim*(one-gamma)+d2x1*dtim*gamma 
   x0=x1
   d1x0=d1x1
   d2x0=d2x1
   IF(j/npri*npri==j)WRITE(11,'(4E12.4)')time,x0(nf(:,nres))
 END DO timesteps

END SUBROUTINE p116
