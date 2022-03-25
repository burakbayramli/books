SUBROUTINE p115(input_file,output_file)
!------------------------------------------------------------------------------
! Program 11.5 Forced vibration of a rectangular elastic solid in plane 
!              strain using uniform 8-node quadrilaterals. Lumped mass.
!              Mesh numbered in y-direction. Complex response.
!------------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,iel,k,nband,ndim=2,ndof=16,nels,neq,nip=9,nlen,nn,nod=8,      &
   nodof=2,npri,nprops=4,np_types,nr,nres,nst=3,nstep,nxe,nye   
 REAL(iwp)::a,area,b,det,dr,dtim,d2=2.0_iwp,omega=0.3,one=1.0_iwp,        &
   time,zero=0.0_iwp
 CHARACTER(len=15)::argv,element='quadrilateral'
!----------------------------- dynamic arrays----------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),nf(:,:),num(:)
 REAL(iwp),ALLOCATABLE::bee(:,:),cm(:,:),coord(:,:),dee(:,:),der(:,:),    &
   deriv(:,:),g_coord(:,:),jac(:,:),km(:,:),mm(:,:),points(:,:),prop(:,:),&
   weights(:),x_coords(:),y_coords(:) 
 COMPLEX(iwp),ALLOCATABLE::kc(:),loads(:)
!-----------------------input and initialisation-------------------------------
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ALLOCATE(nf(nodof,nn),points(nip,ndim),g(ndof), g_coord(ndim,nn),        &
   dee(nst,nst),coord(nod,ndim),jac(ndim,ndim),weights(nip),der(ndim,nod),&
   deriv(ndim,nod),bee(nst,ndof),km(ndof,ndof),num(nod),g_num(nod,nels),  &
   g_g(ndof,nels),mm(ndof,ndof),cm(ndof,ndof),prop(nprops,np_types),      &
   etype(nels),x_coords(nxe+1),y_coords(nye+1))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)x_coords,y_coords,dtim,nstep,npri,nres
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)
 CALL sample(element,points,weights)
 nband=0      
!---------------loop the elements to find bandwidth--------------------
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,'y')
   call num_to_g(num,nf,g)
   g_num(:,iel)=num
   g_g(:,iel)=g
   g_coord(:,num)=transpose(coord)
   IF(nband<bandwidth(g))nband=bandwidth(g)
 END DO elements_1                                                 
 WRITE(11,'(2(A,I5))')                                                    &
   "There are ",neq,"  equations and the half-bandwidth is", nband  
 ALLOCATE(kc(neq*(nband+1)),loads(0:neq))
 kc=(0.0_iwp,0.0_iwp)
!----------------- element stiffness integration and assembly----------------
 elements_2: DO iel=1,nels
   CALL deemat(dee,prop(1,etype(iel)),prop(2,etype(iel)))  
   num=g_num(:,iel)
   coord=transpose(g_coord(:,num)) 
   g=g_g(:,iel)
   area=(coord(5,1)-coord(1,1))*(coord(5,2)-coord(1,2))
   CALL elmat(area,prop(3,etype(iel)),mm)
   km=zero    
   gauss_points_1: DO i=1,nip
     CALL shape_der(der,points,i)
     jac=MATMUL(der,coord)
     det=determinant(jac)
     CALL invert(jac)
     deriv=MATMUL(jac,der)
     CALL beemat(bee,deriv) 
     km=km+MATMUL(MATMUL(TRANSPOSE(bee),dee),bee)*det*weights(i)
   END DO gauss_points_1 
   dr=prop(4,etype(iel))
   cm=km*d2*dr*SQRT(one-dr*dr)
   km=km*(one-d2*dr*dr)-mm*omega**2
   CALL formkc(kc,km,cm,g,neq)
 END DO elements_2
!---------------------complex equation solution------------------------------
 loads=(0.0_iwp,0.0_iwp)
 loads(neq)=(1.0_iwp,0.0_iwp)
 CALL comred(kc,neq)
 CALL comsub(kc,loads)
 a=REAL(loads(neq))
 b=AIMAG(loads(neq))
!----------------------- time stepping loop ----------------------------------
 WRITE(11,'(/A,I5)')" Result at node",nres
 WRITE(11,'(A)')"    time        load        y-disp"
 timesteps: DO i=0,nstep
   time=i*dtim
   IF(i/npri*npri==i)WRITE(11,'(3E12.4)')time,               &
     COS(omega*time),a*COS(omega*time)-b*SIN(omega*time)
 END DO timesteps
END SUBROUTINE p115
