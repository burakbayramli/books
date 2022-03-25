SUBROUTINE p94(input_file,output_file)                
!-------------------------------------------------------------------------
! Program 9.4 Plane strain consolidation analysis of a Biot elastic solid 
!             using 8-node rectangular quadrilaterals for displacements 
!             coupled to 4-node rectangular quadrilaterals for pressures. 
!             Freedoms numbered in order u-v-uw. Incremental load version.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,iel,j,k,loaded_nodes,ndim=2,ndof=16,nels,neq,nip=4,nlen,nlfp, &
   nls,nn,nod=8,nodf=4,nodof=3,npri,nprops=4,np_types,nr,nres,nst=3,nstep,&
   ntot=20,nxe,nye
 CHARACTER(LEN=15)::argv,element='quadrilateral'   
 REAL(iwp)::det,dtim,theta,time,tot_load,zero=0.0_iwp
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),kdiag(:),nf(:,:), &
   no(:),num(:)    
 REAL(iwp),ALLOCATABLE::al(:),ans(:),bee(:,:),c(:,:),coord(:,:),dee(:,:), &
   der(:,:),derf(:,:),deriv(:,:),derivf(:,:),eld(:),fun(:),funf(:),gc(:), &
   g_coord(:,:),jac(:,:),kay(:,:),ke(:,:),km(:,:),kc(:,:),kv(:),lf(:,:),  &
   loads(:),phi0(:),phi1(:),points(:,:),prop(:,:),sigma(:),               &
   store_kc(:,:,:),val(:,:),vol(:),volf(:,:),weights(:),x_coords(:),      &
   y_coords(:)    
!-----------------------input and initialisation--------------------------
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ALLOCATE(dee(nst,nst),points(nip,ndim),coord(nod,ndim),derivf(ndim,nodf),&
   jac(ndim,ndim),kay(ndim,ndim),der(ndim,nod),deriv(ndim,nod),           &
   derf(ndim,nodf),funf(nodf),bee(nst,ndof),km(ndof,ndof),kc(nodf,nodf),  &
   g_g(ntot,nels),ke(ntot,ntot),c(ndof,nodf),x_coords(nxe+1),             &
   y_coords(nye+1),vol(ndof),nf(nodof,nn),g(ntot),volf(ndof,nodf),        &
   g_coord(ndim,nn),g_num(nod,nels),num(nod),weights(nip),                &
   store_kc(nodf,nodf,nels),phi0(nodf),phi1(nodf),prop(nprops,np_types),  &
   etype(nels),eld(ndof),gc(ndim),sigma(nst),fun(nod))
 READ(10,*)prop
 etype=1
 if(np_types>1)READ(10,*)etype
 READ(10,*)x_coords,y_coords
 READ(10,*)dtim,nstep,theta,npri,nres
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)
 ALLOCATE(kdiag(neq),loads(0:neq),ans(0:neq))
 READ(10,*)loaded_nodes
 ALLOCATE(no(loaded_nodes),val(loaded_nodes,ndim))
 READ(10,*)(no(i),val(i,:),i=1,loaded_nodes) 
 READ(10,*)nlfp
 ALLOCATE(lf(2,nlfp))
 READ(10,*)lf
 nls=FLOOR(lf(1,nlfp)/dtim)
 IF(nstep>nls)nstep=nls
 ALLOCATE(al(nstep))
 CALL load_function(lf,dtim,al)
 kdiag=0
!-----------------------loop the elements to find global arrays sizes-----
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,'x')
   g(1:15:2)=nf(1,num(:))
   g(2:16:2)=nf(2,num(:))
   g(17:)=nf(3,num(1:7:2))
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
 CALL sample(element,points,weights)
 loads=zero
 kv=zero
!-----------------------global matrix assembly----------------------------
 elements_2: DO iel=1,nels
   kay=zero
   DO i=1,ndim
     kay(i,i)=prop(i,etype(iel))
   END DO
   CALL deemat(dee,prop(3,etype(iel)),prop(4,etype(iel)))
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   g=g_g(:,iel)
   km=zero
   c=zero
   kc=zero 
 gauss_points_1: DO i=1,nip
!-----------------------elastic solid contribution------------------------
     CALL shape_der(der,points,i)
     jac=MATMUL(der,coord)
     det=determinant(jac)
     CALL invert(jac)
     deriv=MATMUL(jac,der)
     CALL beemat(bee,deriv)
     vol(:)=bee(1,:)+bee(2,:)
     km=km+MATMUL(MATMUL(TRANSPOSE(bee),dee),bee)*det*weights(i)
!-----------------------fluid contribution--------------------------------
     CALL shape_fun(funf,points,i)
     CALL shape_der(derf,points,i)
     derivf=MATMUL(jac,derf)
     kc=kc+MATMUL(MATMUL(TRANSPOSE(derivf),kay),derivf)*det*weights(i)*dtim
     CALL cross_product(vol,funf,volf)
     c=c+volf*det*weights(i)
   END DO gauss_points_1
   store_kc(:,:,iel)=kc
   CALL formke(km,kc,c,ke,theta)
   CALL fsparv(kv,ke,g,kdiag)
 END DO elements_2
 CALL sparin_gauss(kv,kdiag) !---factorise equations----
 !-----------------------time stepping loop--------------------------------
 WRITE(11,'(/A,I5)')" Results at node",nres
 WRITE(11,'(A)')                                                          &
   "    time        load        x-disp      y-disp   porepressure"
 WRITE(11,'(5E12.4)')0.0,0.0,loads(nf(:,nres))
 time_steps: DO j=1,nstep
   tot_load=SUM(al(1:j))
   time=j*dtim
   ans=zero
   elements_3: DO iel=1,nels
     g=g_g(:,iel)
     kc=store_kc(:,:,iel)
     phi0=loads(g(ndof+1:))
     phi1=MATMUL(kc,phi0)
     ans(g(ndof+1:))=ans(g(ndof+1:))+phi1 
   END DO elements_3
!-----------------------apply loading increment---------------------------
   DO i=1,loaded_nodes
     ans(nf(1:2,no(i)))=val(i,:)*al(j)
   END DO
!-----------------------equation solution---------------------------------
   CALL spabac_gauss(kv,ans,kdiag)
   loads=loads+ans
   loads(0)=zero
   IF(j/npri*npri==j)WRITE(11,'(5E12.4)')time,tot_load,loads(nf(:,nres))
!-----------------------recover stresses at nip integrating points--------
!   nip=1DEALLOCATE(points,weights)
!   ALLOCATE(points(nip,ndim),weights(nip))
!   CALL sample(element,points,weights)
!   WRITE(11,'(A,I2,A)')" The integration point (nip=",nip,") stresses are:"
!   WRITE(11,'(A,A)')" Element x-coord     y-coord",                       &
!                    "     sig_x       sig_y       tau_xy" 
!   elements_4: DO iel=1,nelsnum=g_num(:,iel)
!     coord=TRANSPOSE(g_coord(:,num))g=g_g(:,iel)eld=loads(g(:ndof))
!     CALL deemat(dee,prop(3,etype(iel)),prop(4,etype(iel)))
!     gauss_pts_2: DO i=1,nip
!       CALL shape_fun(fun,points,i)CALL shape_der(der,points,i)
!       gc=MATMUL(fun,coord)jac=MATMUL(der,coord)CALL invert(jac)
!       deriv=MATMUL(jac,der)CALL beemat(bee,deriv)
!       sigma=MATMUL(dee,MATMUL(bee,eld))
!       IF(j/npri*npri==j)WRITE(11,'(I5,6E12.4)')iel,gc,sigma
!     END DO gauss_pts_2
!   END DO elements_4
 END DO time_steps
 CALL dismsh(loads,nf,0.05_iwp,g_coord,g_num,argv,nlen,13)
 CALL vecmsh(loads,nf,0.05_iwp,0.1_iwp,g_coord,g_num,argv,nlen,14)

END SUBROUTINE p94
