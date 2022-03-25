SUBROUTINE p113(input_file,output_file)
!-------------------------------------------------------------------------
! Program 11.3 Forced vibration analysis of an elastic solid in plane
!              strain using rectangular 8-node quadrilaterals. Lumped or
!              consistent mass. Mesh numbered in x- or y-direction.
!              Implicit time integration using the "theta" method.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,j,iel,k,loaded_nodes,ndim=2,ndof=16,nels,neq,nip=9,nlen,nn,   &
   nod=8,nodof=2,npri,nprops=3,np_types,nr,nres,nst=3,nstep,nxe,nye
 REAL(iwp)::area,c1,c2,c3,c4,det,dtim,fk,fm,one=1.0_iwp,theta,time,       &
   zero=0.0_iwp
   LOGICAL::consistent=.FALSE.
 CHARACTER(LEN=15)::argv,element='quadrilateral'
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),kdiag(:),nf(:,:), &
   node(:),num(:)   
 REAL(iwp),ALLOCATABLE::bee(:,:),coord(:,:),dee(:,:),der(:,:),deriv(:,:), &
   d1x0(:),d1x1(:),d2x0(:),d2x1(:),ecm(:,:),fun(:),f1(:),g_coord(:,:),    &
   jac(:,:),km(:,:),kv(:),loads(:),mm(:,:),mv(:),points(:,:),prop(:,:),   &
   val(:,:),weights(:),x0(:),x1(:),x_coords(:),y_coords(:)
!-----------------------input and initialisation--------------------------
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ALLOCATE(nf(nodof,nn),points(nip,ndim),g(ndof),g_coord(ndim,nn),         &
   dee(nst,nst),coord(nod,ndim),jac(ndim,ndim),weights(nip),der(ndim,nod),&
   deriv(ndim,nod),bee(nst,ndof),km(ndof,ndof),num(nod),g_num(nod,nels),  &
   g_g(ndof,nels),mm(ndof,ndof),ecm(ndof,ndof),fun(nod),etype(nels),      &
   prop(nprops,np_types),x_coords(nxe+1),y_coords(nye+1))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)x_coords,y_coords,dtim,nstep,theta,npri,nres,fm,fk
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)
 ALLOCATE(x0(0:neq),d1x0(0:neq),x1(0:neq),d2x0(0:neq),loads(0:neq),       &
   d1x1(0:neq),d2x1(0:neq),kdiag(neq))
 READ(10,*)loaded_nodes
 ALLOCATE(node(loaded_nodes),val(loaded_nodes,ndim))
 READ(10,*)(node(i),val(i,:),i=1,loaded_nodes) 
 CALL sample(element,points,weights)
 kdiag=0
!-----------------------loop the elements to find global array sizes------
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,'y')
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
 ALLOCATE(kv(kdiag(neq)),mv(kdiag(neq)),f1(kdiag(neq)))
 WRITE(11,'(2(A,I5))')                                                    &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
 kv=zero
 mv=zero
!-----------------------global stiffness and mass matrix assembly---------
 elements_2: DO iel=1,nels
   CALL deemat(dee,prop(1,etype(iel)),prop(2,etype(iel)))
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   g=g_g(:,iel)
   km=zero
   mm=zero
   area=zero
   gauss_pts_1: DO i=1,nip
     CALL shape_der(der,points,i)
     CALL shape_fun(fun,points,i)
     jac=MATMUL(der,coord)
     det=determinant(jac)
     CALL invert(jac)
     deriv=MATMUL(jac,der)
     CALL beemat(bee,deriv)
     km=km+MATMUL(MATMUL(TRANSPOSE(bee),dee),bee)*det*weights(i)
     area=area+det*weights(i) 
     IF(consistent)THEN
       CALL ecmat(ecm,fun,ndof,nodof)
       mm=mm+ecm*det*weights(i)*prop(3,etype(iel))
     END IF
   END DO gauss_pts_1
   IF(.NOT.consistent)CALL elmat(area,prop(3,etype(iel)),mm)
   CALL fsparv(kv,km,g,kdiag)
   CALL fsparv(mv,mm,g,kdiag)
 END DO elements_2 
!-----------------------initial conditions and factorise equations--------
 x0=zero
 d1x0=zero
 d2x0=zero
 c1=(one-theta)*dtim
 c2=fk-c1
 c3=fm+one/(theta*dtim)
 c4=fk+theta*dtim
 f1=c3*mv+c4*kv
 CALL sparin(f1,kdiag)
 time=zero
!-----------------------time stepping loop--------------------------------
 WRITE(11,'(/A,I5)')" Result at node",nres
 WRITE(11,'(A)')"    time        load        x-disp      y-disp"
 WRITE(11,'(4E12.4)')time,load(time),x0(nf(:,nres))
 timesteps: DO j=1,nstep
   time=time+dtim
   loads=zero
   x1=c3*x0+d1x0/theta
   DO i=1,loaded_nodes
     loads(nf(:,node(i)))=                                                &
       val(i,:)*(theta*dtim*load(time)+c1*load(time-dtim))
   END DO
   CALL linmul_sky(mv,x1,d1x1,kdiag)
   d1x1=loads+d1x1
   loads=c2*x0
   CALL linmul_sky(kv,loads,x1,kdiag)
   x1=x1+d1x1
   CALL spabac(f1,x1,kdiag)
   d1x1=(x1-x0)/(theta*dtim)-d1x0*(one-theta)/theta
   d2x1=(d1x1-d1x0)/(theta*dtim)-d2x0*(one-theta)/theta
   x0=x1
   d1x0=d1x1
   d2x0=d2x1
   IF(j/npri*npri==j)WRITE(11,'(4E12.4)')time,load(time),x0(nf(:,nres))
 END DO timesteps
 
CONTAINS
FUNCTION load(t) RESULT(load_result)
!-----------------------Load-time function--------------------------------
 IMPLICIT NONE     
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::t
 REAL(iwp)::load_result
 load_result=COS(0.3_iwp*t)
RETURN
END FUNCTION load
END SUBROUTINE p113
