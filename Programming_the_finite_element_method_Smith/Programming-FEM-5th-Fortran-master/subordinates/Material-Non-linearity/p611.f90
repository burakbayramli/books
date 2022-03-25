SUBROUTINE p611(input_file,output_file)
!-------------------------------------------------------------------------
! Program 6.11 Axisymmetric `undrained' strain of an elastic-plastic
!             (Mohr-Coulomb) solid using 8-node rectangular
!             quadrilaterals. Viscoplastic strain method.
!-------------------------------------------------------------------------
 USE main
 USE geom
  IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,incs,iters,iy,k,limit,ndim=2,ndof=16,nels, &
   neq,nip=4,nlen,nn,nod=8,nodof=2,nr,nst=4,nxe,nye 
 REAL(iwp)::bulk,c,cons,det,dq1,dq2,dq3,dsbar,dt,d4=4.0_iwp,              &
   d180=180.0_iwp,e,f,lode_theta,one=1.0_iwp,phi,pi,presc,psi,sigm,snph,  &
   penalty=1.e20_iwp,tol,two=2.0_iwp,v,zero=0.0_iwp
 CHARACTER(LEN=15)::argv,element='quadrilateral'
 LOGICAL::converged
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::g(:),g_g(:,:),g_num(:,:),kdiag(:),nf(:,:),no(:),    &
   node(:),num(:),sense(:)
 REAL(iwp),ALLOCATABLE::bdylds(:),bee(:,:),bload(:),coord(:,:),dee(:,:),  &
   der(:,:),deriv(:,:),devp(:),eld(:),eload(:),eps(:),erate(:),           &
   etensor(:,:,:),evp(:),evpt(:,:,:),flow(:,:),fun(:),gc(:),g_coord(:,:), &
   jac(:,:),km(:,:),kv(:),loads(:),m1(:,:),m2(:,:),m3(:,:),oldis(:),      &
   points(:,:),pore(:,:),sigma(:),storkv(:),stress(:),tensor(:,:,:),      &
   totd(:),weights(:),x_coords(:),y_coords(:)
!-----------------------input and initialisation--------------------------

 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,phi,c,psi,e,v,bulk,cons 
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ALLOCATE(nf(nodof,nn),points(nip,ndim),weights(nip),g_coord(ndim,nn),    &
   x_coords(nxe+1),y_coords(nye+1),num(nod),evpt(nst,nip,nels),           &
   coord(nod,ndim),g_g(ndof,nels),tensor(nst,nip,nels),fun(nod),          &
   etensor(nst,nip,nels),dee(nst,nst),pore(nip,nels),stress(nst),         &
   jac(ndim,ndim),der(ndim,nod),deriv(ndim,nod),g_num(nod,nels),          &
   bee(nst,ndof),km(ndof,ndof),eld(ndof),eps(nst),sigma(nst),bload(ndof), &
   eload(ndof),erate(nst),evp(nst),devp(nst),g(ndof),m1(nst,nst),         &
   m2(nst,nst),m3(nst,nst),flow(nst,nst),gc(ndim))
 READ(10,*)x_coords,y_coords
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)
 ALLOCATE(kdiag(neq),loads(0:neq),bdylds(0:neq),oldis(0:neq),totd(0:neq))
!-----------------------loop the elements to find global arrays sizes-----
 kdiag=0
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
 ALLOCATE(kv(kdiag(neq)))
 WRITE(11,'(2(A,I7))')                                                    &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
!-----------------------add fluid bulk modulus effective dee matrix-------
 CALL deemat(dee,e,v)
 pi=ACOS(-one)
 DO i=1,nst
 DO k=1,nst
   IF(i/=3.AND.k/=3)dee(i,k)=dee(i,k)+bulk
 END DO
 END DO
 snph=SIN(phi*pi/d180)
 dt=d4*(one+ v)*(one-two*v)/(e*(one-two*v+snph*snph))
 CALL sample(element,points,weights)
 kv=zero
 tensor=zero
 etensor=zero
!-----------------------element stiffness integration and assembly--------
 elements_2: DO iel=1,nels
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   g=g_g(:,iel)
   km=zero
   gauss_pts_1: DO i=1,nip
     CALL shape_fun(fun,points,i)
     CALL bee8(bee,coord,points(i,1),points(i,2),det)
     gc=MATMUL(fun,coord)
     bee(4,1:ndof-1:2)=fun(:)/gc(1)
     km=km+MATMUL(MATMUL(TRANSPOSE(bee),dee),bee)*det*weights(i)*gc(1)
     tensor(1:2,i,iel)=cons
     tensor(4,i,iel)=cons
   END DO gauss_pts_1
   CALL fsparv(kv,km,g,kdiag)
 END DO elements_2
!-----------------------read displacement data and factorise equations----
 READ(10,*)fixed_freedoms
 IF(fixed_freedoms/=0)THEN
   ALLOCATE(node(fixed_freedoms),sense(fixed_freedoms),no(fixed_freedoms),&
     storkv(fixed_freedoms))
   READ(10,*)(node(i),sense(i),i=1,fixed_freedoms)
   DO i=1,fixed_freedoms
     no(i)=nf(sense(i),node(i))
   END DO
   kv(kdiag(no))=kv(kdiag(no))+penalty
   storkv=kv(kdiag(no))
 END IF
 CALL sparin(kv,kdiag)
 CALL deemat(dee,e,v)
!-----------------------displacement increment loop-----------------------
 READ(10,*)tol,limit,incs,presc
 WRITE(11,'(/A)')"  step   disp      dev stress  pore press  iters"
 oldis=zero
 totd=zero
 disp_incs: DO iy=1,incs
   iters=0
   bdylds=zero
   evpt=zero
!-----------------------plastic iteration loop----------------------------
   its: DO
     iters=iters+1
     WRITE(*,'(A,E11.3,A,I4)')"  displacement",iy*presc,"  iteration",iters
     loads=zero
     loads(no)=storkv*presc
     loads=loads+bdylds 
     CALL spabac(kv,loads,kdiag)
!-----------------------check plastic convergence-------------------------
     CALL checon(loads,oldis,tol,converged)
     IF(iters==1)converged=.FALSE.
!-----------------------go round the Gauss points ------------------------
     elements_3: DO iel=1,nels
       bload=zero
       num=g_num(:,iel)
       coord=TRANSPOSE(g_coord(:,num))
       g=g_g(:,iel)
       eld=loads(g)
       gauss_pts_2: DO i=1,nip
         CALL shape_fun(fun,points,i)
         CALL bee8(bee,coord,points(i,1),points(i,2),det)
         gc=MATMUL(fun,coord)
         bee(4,1:ndof-1:2)=fun(:)/gc(1)
         eps=MATMUL(bee,eld)
         eps=eps-evpt(:,i,iel)
         sigma=MATMUL(dee,eps)
         stress=sigma+tensor(:,i,iel)
         CALL invar(stress,sigm,dsbar,lode_theta)
!-----------------------check whether yield is violated-------------------
         CALL mocouf(phi,c,sigm,dsbar,lode_theta,f)
         IF(f>=zero)THEN
           CALL mocouq(psi,dsbar,lode_theta,dq1,dq2,dq3) 
           CALL formm(stress,m1,m2,m3)
           flow=f*(m1*dq1+m2*dq2+m3*dq3)
           erate=MATMUL(flow,stress)
           evp=erate*dt
           evpt(:,i,iel)=evpt(:,i,iel)+evp
           devp=MATMUL(dee,evp)
           eload=MATMUL(devp,bee)
           bload=bload+eload*det*weights(i)*gc(1)
         END IF
!------update the Gauss Point stresses and calculate pore pressures-------
         IF(converged.OR.iters==limit)THEN
           tensor(:,i,iel)=stress
           etensor(:,i,iel)=etensor(:,i,iel)+eps+evpt(:,i,iel)
           pore(i,iel)=(etensor(1,i,iel)+etensor(2,i,iel)+                &
             etensor(4,i,iel))*bulk
         END IF
       END DO gauss_pts_2
!-----------------------compute the total bodyloads vector ---------------
       bdylds(g)=bdylds(g)+bload
       bdylds(0)=zero
     END DO elements_3
     IF(converged.OR.iters==limit)EXIT
   END DO its
   totd=totd+loads
   WRITE(11,'(I5,3E12.4,I5)')iy,totd(no(1)),dsbar, pore(1,1),iters
   IF(iters==limit)EXIT
 END DO disp_incs
 CALL dismsh(loads,nf,0.05_iwp,g_coord,g_num,argv,nlen,13)
 CALL vecmsh(loads,nf,0.05_iwp,0.1_iwp,g_coord,g_num,argv,nlen,14)

END SUBROUTINE p611
