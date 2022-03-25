SUBROUTINE p118(input_file,output_file)
!-------------------------------------------------------------------------
! Program 11.8 Forced vibration analysis of an elastic-plastic (Von Mises)
!              solid in plane strain using rectangular 8-node quadrilateral
!              elements. Lumped mass. Mesh numbered in x- or y-direction.
!              Explicit time integration.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,j,iel,k,loaded_nodes,ndim=2,ndof=16,nels,neq,nip=4,nlen,nn,   &
   nod=8,nodof=2,npri,nprops=4,np_types,nr,nres,nst=4,nstep,nxe,nye
 REAL(iwp)::area,det,dsbar,dtim,f,fac,fmax,fnew,lode_theta,one=1.0_iwp,   &
   pt5=0.5_iwp,sigm,time,zero=0.0_iwp
 CHARACTER(LEN=15)::argv,element='quadrilateral'
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),nf(:,:),node(:),  &
   num(:)
 REAL(iwp),ALLOCATABLE::bdylds(:),bee(:,:),bload(:),coord(:,:),dee(:,:),  &
   der(:,:),deriv(:,:),d1x1(:),d2x1(:),eld(:),eload(:),eps(:),            &
   etensor(:,:,:),diag(:),g_coord(:,:),jac(:,:),mm(:,:),pl(:,:),          &
   points(:,:),prop(:,:),sigma(:),stress(:),tensor(:,:,:),val(:,:),       &
   weights(:),x1(:),x_coords(:),y_coords(:)
!-----------------------input and initialisation--------------------------
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ALLOCATE(nf(nodof,nn),points(nip,ndim),weights(nip),g_coord(ndim,nn),    &
   num(nod),dee(nst,nst),tensor(nst,nip,nels),coord(nod,ndim),pl(nst,nst),&
   etensor(nst,nip,nels),jac(ndim,ndim),der(ndim,nod),deriv(ndim,nod),    &
   g_num(nod,nels),bee(nst,ndof),eld(ndof),eps(nst),sigma(nst),           &
   mm(ndof,ndof),bload(ndof),eload(ndof),g(ndof),stress(nst),etype(nels), &
   g_g(ndof,nels),x_coords(nxe+1),y_coords(nye+1),prop(nprops,np_types))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)x_coords,y_coords,dtim,nstep,npri,nres
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)
 ALLOCATE(bdylds(0:neq),x1(0:neq),d1x1(0:neq),d2x1(0:neq),diag(0:neq))
 READ(10,*)loaded_nodes
 ALLOCATE(node(loaded_nodes),val(loaded_nodes,ndim))
 READ(10,*)(node(i),val(i,:),i=1,loaded_nodes)
!-----------------------loop the elements to set up global geometry ------
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,'y')
   CALL num_to_g(num,nf,g)
   g_num(:,iel)=num 
   g_coord(:,num)=TRANSPOSE(coord)
   g_g(:,iel)=g
 END DO elements_1
 CALL mesh(g_coord,g_num,argv,nlen,12)
 WRITE(11,'(A,I5,A)')"There are",neq," equations"
!-----------------------initial conditions--------------------------------
 tensor=zero
 etensor=zero
 x1=zero
 d1x1=zero
 d2x1=zero
 diag=zero 
 CALL sample(element,points,weights)
 time=zero
!-----------------------time stepping loop--------------------------------
 WRITE(11,'(/A,I5)')" Result at node",nres
 WRITE(11,'(A)')"    time        load        x-disp      y-disp"
 WRITE(11,'(4E12.4)')time,load(time),x1(nf(:,nres))
 time_steps: DO j=1,nstep
   fmax=zero
   time=time+dtim
   x1=x1+dtim*d1x1+pt5*dtim**2*d2x1
   bdylds=zero
!-----------------------go round the Gauss Points ------------------------
   elements_2: DO iel=1,nels
     num=g_num(:,iel)
     coord=TRANSPOSE(g_coord(:,num))
     g=g_g(:,iel)
     area=zero
     bload=zero
     eld=x1(g)
     gauss_pts_1: DO i=1,nip
       CALL shape_der(der,points,i)
       CALL deemat(dee,prop(1,etype(iel)),prop(2,etype(iel)))
       jac=MATMUL(der,coord)
       det=determinant(jac)
       area=area+det*weights(i)
       CALL invert(jac)
       deriv=MATMUL(jac,der)
       CALL beemat(bee,deriv)
       eps=MATMUL(bee,eld)
       eps=eps-etensor(:,i,iel)
       sigma=MATMUL(dee,eps)
       stress=sigma+tensor(:,i,iel) 
       CALL invar(stress,sigm,dsbar,lode_theta)
       fnew=dsbar-prop(4,etype(iel))
!-----------------------check whether yield is violated-------------------
       IF(fnew>=zero)THEN
         stress=tensor(:,i,iel)
         CALL invar(stress,sigm,dsbar,lode_theta)
         f=dsbar-prop(4,etype(iel))
         fac=fnew/(fnew-f)
         stress=tensor(:,i,iel)+(one-fac)*sigma
         CALL vmdpl(dee,stress,pl)
         dee=dee-fac*pl
       END IF
       sigma=MATMUL(dee,eps)
       sigma=sigma+tensor(:,i,iel)
       CALL invar(sigma,sigm,dsbar,lode_theta)
       f=dsbar-prop(4,etype(iel))
       IF(f>fmax)fmax=f
       eload=MATMUL(sigma,bee)
       bload=bload+eload*det*weights(i)
!-----------------------update the Gauss Point stresses and strains-------
       tensor(:,i,iel)=sigma
       etensor(:,i,iel)=etensor(:,i,iel)+eps
     END DO gauss_pts_1
     bdylds(g)=bdylds(g)-bload
     IF(j==1)THEN
       CALL elmat(area,prop(3,etype(iel)),mm)
       CALL formlump(diag,mm,g)
     END IF
   END DO elements_2
   bdylds(0)=zero
   DO i=1,loaded_nodes
     bdylds(nf(:,node(i)))=bdylds(nf(:,node(i)))+val(i,:)*load(time)
   END DO
   bdylds(1:)=bdylds(1:)/diag(1:)
   d1x1=d1x1+(d2x1+bdylds)*pt5*dtim
   d2x1=bdylds
   WRITE(*,'(A,I6,A,F8.4)')"  time step",j,"    F_max",fmax
   IF(j/npri*npri==j)WRITE(11,'(4E12.4)')time,load(time),x1(nf(:,nres))
 END DO time_steps

CONTAINS
FUNCTION load(t) RESULT(load_result)
!-----------------------Load-time function--------------------------------
 IMPLICIT NONE     
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::t
 REAL(iwp)::load_result
 load_result=-180.0_iwp
RETURN
END FUNCTION load
END SUBROUTINE p118
