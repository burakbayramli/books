SUBROUTINE p52(input_file,output_file)
!-------------------------------------------------------------------------
! Program 5.2 Non-axisymmetric analysis of an axisymmetric elastic solid
!             using 8-node rectangular quadrilaterals. Mesh numbered in
!             r- or z- direction.
!-------------------------------------------------------------------------
 USE main 
 USE geom 
 IMPLICIT NONE
 CHARACTER(len=70),INTENT(IN) :: input_file
 CHARACTER(len=70),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,iel,iflag,k,loaded_nodes,lth,ndim=2,ndof=24,nels,neq,nip=4,   &
   nlen,nod=8,nodof=3,nn,nprops=2,np_types,nr,nre,nst=6,nze 
 REAL(iwp)::ca,chi,det,one=1.0_iwp,pi,radius,sa,zero=0.0_iwp
 CHARACTER(LEN=15)::argv,element='quadrilateral'
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),kdiag(:),nf(:,:), &
   num(:)  
 REAL(iwp),ALLOCATABLE::bee(:,:),coord(:,:),dee(:,:),der(:,:),deriv(:,:), &
 eld(:),fun(:),gc(:),g_coord(:,:),jac(:,:),km(:,:),kv(:),loads(:),        &
 points(:,:),prop(:,:),r_coords(:),sigma(:),weights(:),z_coords(:)  
!-----------------------input and initialisation--------------------------

 OPEN(10,FILE=input_file) 
 OPEN(11,FILE=output_file)
 READ(10,*)nre,nze,lth,iflag,chi,np_types 
 CALL mesh_size(element,nod,nels,nn,nre,nze)
 ALLOCATE(nf(nodof,nn),points(nip,ndim),g(ndof),g_coord(ndim,nn),         &
   dee(nst,nst),coord(nod,ndim),fun(nod),jac(ndim,ndim),eld(ndof),        &
   weights(nip),der(ndim,nod),deriv(ndim,nod),bee(nst,ndof),km(ndof,ndof),&
   sigma(nst),num(nod),g_num(nod,nels),g_g(ndof,nels),gc(ndim),           &
   r_coords(nre+1),z_coords(nze+1),prop(nprops,np_types),etype(nels))
 READ(10,*)prop 
 etype=1 
 IF(np_types>1)read(10,*)etype
 READ(10,*)r_coords,z_coords
 nf=1 
 READ(10,*)nr,(k,nf(:,k),i=1,nr) 
 CALL formnf(nf) 
 neq=MAXVAL(nf) 
 ALLOCATE(kdiag(neq),loads(0:neq))
 pi=ACOS(-one) 
 chi=chi*pi/180.0_iwp 
 ca=COS(chi) 
 sa=SIN(chi) 
 kdiag=0
!-----------------------loop the elements to find global arrays sizes-----
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,r_coords,z_coords,coord,num,'r')
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
 ALLOCATE(kv(kdiag(neq)))
 WRITE(11,'(2(A,I5))')                                                    &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
!-----------------------element stiffness integration and assembly--------
 CALL sample(element,points,weights) 
 kv=zero
 elements_2: DO iel=1,nels
   CALL deemat(dee,prop(1,etype(iel)),prop(2,etype(iel))) 
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num)) 
   g=g_g(:,iel) 
   km=zero
   gauss_pts_1: DO i=1,nip
     CALL shape_fun(fun,points,i) 
     CALL shape_der(der,points,i)
     jac=MATMUL(der,coord) 
     det=determinant(jac) 
     CALL invert(jac)
     deriv=MATMUL(jac,der)
     CALL bmat_nonaxi(bee,radius,coord,deriv,fun,iflag,lth)
     km=km+MATMUL(MATMUL(TRANSPOSE(bee),dee),bee)*det*weights(i)*radius
   END DO gauss_pts_1
   CALL fsparv(kv,km,g,kdiag)
 END DO elements_2                                                            
 loads=zero 
 READ(10,*)loaded_nodes,(k,loads(nf(:,k)),i=1,loaded_nodes)
!-----------------------equation solution---------------------------------
 CALL sparin(kv,kdiag) 
 CALL spabac(kv,loads,kdiag) 
 loads(0)=zero
 WRITE(11,'(/A)')"  Node   r-disp      z-disp      t-disp"
 DO k=1,nn 
   WRITE(11,'(I5,3E12.4)')k,loads(nf(:,k)) 
 END DO
!-----------------------recover stresses at nip integrating points--------
 nip=1 
 DEALLOCATE(points,weights) 
 ALLOCATE(points(nip,ndim),weights(nip))
 CALL sample(element,points,weights)
 WRITE(11,'(/A,I2,A)')" The integration point (nip=",nip,") stresses are:"
 WRITE(11,'(A,A)')" Element      r-coord     z-coord",                    &
   "     sig_r       sig_z       sig_t" 
 WRITE(11,'(A,A)')"                                 ",                    &
   "     tau_rz      tau_zt      tau_tr" 
 elements_3: DO iel=1,nels
   CALL deemat(dee,prop(1,etype(iel)),prop(2,etype(iel))) 
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num)) 
   g=g_g(:,iel) 
   eld=loads(g)
   int_pts_2: DO i=1,nip
     CALL shape_fun(fun,points,i) 
     CALL shape_der(der,points,i)
     gc=MATMUL(fun,coord) 
     jac=MATMUL(der,coord) 
     CALL invert(jac)
     deriv=MATMUL(jac,der)
     CALL bmat_nonaxi(bee,radius,coord,deriv,fun,iflag,lth)
     bee(1:4,:)=bee(1:4,:)*ca 
     bee(5:6,:)=bee(5:6,:)*sa
     sigma=MATMUL(dee,MATMUL(bee,eld)) 
     WRITE(11,'(I5,5X,5E12.4/34X,3E12.4)')iel,gc,sigma(:3),sigma(4:6)
   END DO int_pts_2
 END DO elements_3
 CALL dismsh(loads,nf,0.05_iwp,g_coord,g_num,argv,nlen,13)
 CALL vecmsh(loads,nf,0.05_iwp,0.1_iwp,g_coord,g_num,argv,nlen,14)

END SUBROUTINE p52
