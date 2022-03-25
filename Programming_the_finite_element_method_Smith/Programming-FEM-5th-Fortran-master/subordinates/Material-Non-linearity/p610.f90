SUBROUTINE p610(input_file,output_file)                       
!------------------------------------------------------------------------
! Program 6.10 Plane strain construction of an elastic-plastic
!             (Mohr-Coulomb) excavation in layers using 8-node
!             quadrilaterals. Viscoplastic strain method.
!------------------------------------------------------------------------
 USE main
 USE geom
  IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,iel,ii,incs,iters,iy,k,layers,limit,ndim=2,ndof=16,nels,neq,  &
   nip=4,nlen,nn,nod=8,nodof=2,noexe,nouts,nprops=7,np_types,nr,nst=4,    &
   ntote
 REAL(iwp)::c,ddt,det,dq1,dq2,dq3,dsbar,dt,d4=4.0_iwp,d180=180.0_iwp,e,f, &
   gamma,lode_theta,one=1.0_iwp,phi,pi,psi,sigm,snph,start_dt=1.e15_iwp,  &
   tol,two=2.0_iwp,v,zero=0.0_iwp
 CHARACTER(LEN=15)::argv,element='quadrilateral'
 LOGICAL::converged          
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),exele(:),g(:),g_num(:,:),kdiag(:),lnf(:,:),&
   nf(:,:),no(:),num(:),solid(:),totex(:)
 REAL(iwp),ALLOCATABLE::bdylds(:),bee(:,:),bload(:),coord(:,:),dee(:,:),  &
   der(:,:),deriv(:,:),devp(:),eld(:),eload(:),eps(:),erate(:),evp(:),    &
   evpt(:,:,:),exc_loads(:),flow(:,:),fun(:),gc(:),g_coord(:,:),jac(:,:), &
   km(:,:),kv(:),loads(:),m1(:,:),m2(:,:),m3(:,:),oldis(:),points(:,:),   &
   prop(:,:),stress(:),tensor(:,:,:),tot_d(:,:),weights(:)
!-----------------------input and initialisation--------------------------

 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nels,nn,np_types
 ALLOCATE(prop(nprops,np_types),etype(nels)) 
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 ALLOCATE(nf(nodof,nn),points(nip,ndim),weights(nip),g_coord(ndim,nn),    &
   num(nod),dee(nst,nst),evpt(nst,nip,nels),coord(nod,ndim),fun(nod),     &
   solid(nels),jac(ndim,ndim),der(ndim,nod),deriv(ndim,nod),              &
   g_num(nod,nels),bee(nst,ndof),km(ndof,ndof),eld(ndof),eps(nst),        &
   totex(nels),bload(ndof),eload(ndof),erate(nst),evp(nst),devp(nst),     &
   g(ndof),m1(nst,nst),m2(nst,nst),m3(nst,nst),flow(nst,nst),stress(nst), &
   tot_d(nodof,nn),gc(ndim),tensor(nst,nip,nels),lnf(nodof,nn))
!-----------------------read geometry and connectivity--------------------
 READ(10,*)g_coord,g_num
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
!---------------lnf is the current nf at each stage of excavation---------
 lnf=nf
 CALL formnf(lnf)
 neq=MAXVAL(lnf)
 WRITE(11,'(A,I5)')" The initial number of elements is:",nels
 WRITE(11,'(A,I5)')" The initial number of freedoms is:",neq
!--------set up the global node numbers and global nodal coordinates------
!-----------------------loop the elements to set starting stresses--------
 CALL sample(element,points,weights)
 elements_0: DO iel=1,nels
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num)) 
   gamma=prop(4,etype(iel))
   gauss_pts_0: DO i=1,nip
     CALL shape_fun(fun,points,i)
     gc=MATMUL(fun,coord)
     tensor(2,i,iel)=gc(2)*gamma
     tensor(1,i,iel)=prop(7,etype(iel))*tensor(2,i,iel)
     tensor(4,i,iel)=tensor(1,i,iel)
     tensor(3,i,iel)=zero
   END DO gauss_pts_0
 END DO elements_0
 tot_d=zero
 ntote=0
 solid=1
 totex=0
 pi=ACOS(-one)
!-----------------------excavate a layer----------------------------------
 READ(10,*)nouts
 ALLOCATE(no(nouts))
 READ(10,*)no,tol,limit,incs,layers
 layer_number: DO ii=1,layers
   WRITE(11,'(/A,I5)')" Excavation number",ii
!-----------------------read elements to be removed-----------------------
   READ(10,*)noexe
   ALLOCATE(exele(noexe))
   READ(10,*)exele
   solid(exele)=0
   CALL exc_nods(noexe,exele,g_num,totex,ntote,nf)
   lnf=nf 
   CALL formnf(lnf)
   neq=MAXVAL(lnf)
   ALLOCATE(kdiag(neq),exc_loads(0:neq),bdylds(0:neq),oldis(0:neq),       &
     loads(0:neq))
     WRITE(11,'(3(A,I5))')" There are",neq," freedoms"
   WRITE(11,'(3(A,I5))')" There are",nels-ntote," elements after",        &
     noexe," were removed"
     kdiag=0
!-----------------------loop the elements to find global arrays sizes-----
   elements_1: DO iel=1,nels
     num=g_num(:,iel)
     CALL num_to_g(num,lnf,g)
     CALL fkdiag(kdiag,g)
   END DO elements_1
   DO i=2,neq
     kdiag(i)=kdiag(i)+kdiag(i-1)
   END DO 
   ALLOCATE(kv(kdiag(neq)))
   exc_loads=zero
!-----------------------calculate excavation load ------------------------
   elements_2: DO iel=1,noexe
     k=exele(iel)
     gamma=prop(4,etype(k))
     bload=zero
     eld=zero
     num=g_num(:,k)
     CALL num_to_g(num,lnf,g) 
     coord=TRANSPOSE(g_coord(:,num))
     gauss_pts_2: DO i=1,nip
       CALL shape_fun(fun,points,i)
       stress=tensor(:,i,k)
       CALL bee8(bee,coord,points(i,1),points(i,2),det)
       eload=MATMUL(stress,bee)
       bload=bload+eload*det*weights(i)
       eld(nodof:ndof:nodof)=eld(nodof:ndof:nodof)+fun(:)*det*weights(i)
     END DO gauss_pts_2
     exc_loads(g)=exc_loads(g)+eld*gamma+bload
   END DO elements_2
   exc_loads(0)=zero
!-----------------------element stiffness integration and assembly--------
   kv=zero
   dt=start_dt
   elements_3: DO iel=1,nels
     IF(solid(iel)==0)THEN
       e=zero
     ELSE
       phi=prop(1,etype(iel))
       e=prop(5,etype(iel))
       v=prop(6,etype(iel))
       snph=SIN(phi*pi/d180)
       ddt=d4*(one+v)*(one-two*v)/(e*(one-two*v+snph*snph)) 
       IF(ddt<dt)dt=ddt
     END IF
     km=zero
     eld=zero
     CALL deemat(dee,e,v)
     num=g_num(:,iel)
     CALL num_to_g(num,lnf,g)
     coord=TRANSPOSE(g_coord(:,num))
     gauss_pts_3: DO i=1,nip
       CALL bee8(bee,coord,points(i,1),points(i,2),det)
       km=km+MATMUL(MATMUL(TRANSPOSE(bee),dee),bee)*det*weights(i)
     END DO gauss_pts_3
     CALL fsparv(kv,km,g,kdiag)
   END DO elements_3
!-----------------------factorise sand factor excavation load by incs-----
   CALL sparin(kv,kdiag)
   exc_loads=exc_loads/incs
!-----------------------apply excavation loads incrementally--------------
   load_incs: DO iy=1,incs
     iters=0
     oldis=zero
     bdylds=zero
     evpt=zero
!-----------------------iteration loop -----------------------------------
     its: DO
       iters=iters+1
       WRITE(*,'(A,I3,A,I3,A,I4)')"  excavation",ii,"  increment",iy,     &
         "  iteration",iters
       loads=exc_loads+bdylds
       CALL spabac(kv,loads,kdiag)
!-----------------------check convergence---------------------------------
       CALL checon(loads,oldis,tol,converged) 
       IF(iters==1)converged=.FALSE.
       IF(converged.OR.iters==limit)THEN
         bdylds=zero
         DO k=1,nn
         DO i=1,nodof
            IF(lnf(i,k)/=0)tot_d(i,k)=tot_d(i,k)+loads(lnf(i,k))
         END DO
         END DO
       END IF
!-----------------------go round the Gauss Points-------------------------
       elements_4: DO iel=1,nels
         phi=prop(1,etype(iel))
         c=prop(2,etype(iel)) 
         psi=prop(3,etype(iel))
         e=prop(5,etype(iel))
         v=prop(6,etype(iel))
         IF(solid(iel)==0)e=zero
         bload=zero
         CALL deemat(dee,e,v)
         num=g_num(:,iel)
         CALL num_to_g(num,lnf,g)
         coord=TRANSPOSE(g_coord(:,num))
         eld=loads(g)
         gauss_pts_4: DO i=1,nip
           CALL bee8(bee,coord,points(i,1),points(i,2),det)
           eps=MATMUL(bee,eld)
           eps=eps-evpt(:,i,iel)
           stress=tensor(:,i,iel)+MATMUL(dee,eps)
!-----------------------air element stresses are zero---------------------
           IF(solid(iel)==0)stress=zero
           CALL invar(stress,sigm,dsbar,lode_theta)
!-----------------------check whether yield is violated-------------------
           CALL mocouf(phi,c,sigm,dsbar,lode_theta,f)
           IF(converged.OR.iters==limit)THEN
             devp=stress
           ELSE
             IF(f>=zero)THEN
             CALL mocouq(psi,dsbar,lode_theta,dq1,dq2,dq3)
               CALL formm(stress,m1,m2,m3)
               flow=f*(m1*dq1+m2*dq2+m3*dq3)
               erate=MATMUL(flow,stress)
               evp=erate*dt
               evpt(:,i,iel)=evpt(:,i,iel)+evp
               devp=MATMUL(dee,evp)
             END IF
           END IF
           IF(f>=zero.OR.(converged.OR.iters==limit))THEN
             eload=MATMUL(devp,bee)
             bload=bload+eload*det*weights(i)
           END IF
!-----------------------if appropriate update the Gauss point stresses----
           IF(converged.OR.iters==limit)tensor(:,i,iel)=stress
         END DO gauss_pts_4
!-----------------------compute the total bodyloads vector ---------------
         bdylds(g)=bdylds(g)+bload
         bdylds(0)=zero
       END DO elements_4
       IF(converged.OR.iters==limit)EXIT
     END DO its
     WRITE(11,'(A,I3,A,I5,A)')" Increment",iy," took",iters,              &
                              " iterations to converge"
     IF(iy==incs.OR.iters==limit)THEN
       WRITE(11,'(A)') "  Node   x-disp      y-disp"
       DO i=1,nouts
         WRITE(11,'(I5,2E12.4)')no(i),tot_d(:,no(i))
       END DO
       EXIT
     END IF
   END DO load_incs
   IF(ii==layers.OR.iters==limit)EXIT
   DEALLOCATE(kdiag,kv,exc_loads,bdylds,oldis,loads,exele)
 END DO layer_number
 loads(lnf(1,:))=tot_d(1,:)
 loads(lnf(2,:))=tot_d(2,:)
 g_num(:,totex(:ntote))=0
 CALL mesh(g_coord,g_num,argv,nlen,12) 
 CALL dismsh(loads,lnf,0.1_iwp,g_coord,g_num,argv,nlen,13)
 CALL vecmsh(loads,lnf,0.1_iwp,0.1_iwp,g_coord,g_num,argv,nlen,14)

END SUBROUTINE p610
