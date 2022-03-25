SUBROUTINE p61(input_file,output_file)
!-------------------------------------------------------------------------
! Program 6.1 Plane strain bearing capacity analysis of an elastic-plastic
!             (von Mises) material using 8-node rectangular
!             quadrilaterals. Viscoplastic strain method.
!-------------------------------------------------------------------------
 USE main 
 USE geom 
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,iel,incs,iters,iy,k,limit,loaded_nodes,ndim=2,ndof=16,nels,   &
   neq,nip=4,nlen,nn,nod=8,nodof=2,nprops=3,np_types,nr,nst=4,nxe,nye 
 REAL(iwp)::ddt,det,dq1,dq2,dq3,dsbar,dt=1.0e15_iwp,d3=3.0_iwp,d4=4.0_iwp,&
   end_time,f,lode_theta,one=1.0_iwp,ptot,sigm,tol,start_time,            &
   two=2.0_iwp,zero=0.0_iwp
 CHARACTER(LEN=15)::argv,element='quadrilateral' 
 LOGICAL::converged
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),kdiag(:),nf(:,:), &
   node(:),num(:)    
 REAL(iwp),ALLOCATABLE::bdylds(:),bee(:,:),bload(:),coord(:,:),dee(:,:),  &
   der(:,:),deriv(:,:),devp(:),eld(:),eload(:),eps(:),erate(:),evp(:),    &
   evpt(:,:,:),flow(:,:),g_coord(:,:),jac(:,:),km(:,:),kv(:),loads(:),    &
   m1(:,:),m2(:,:),m3(:,:),oldis(:),points(:,:),prop(:,:),qinc(:),        &
   sigma(:),stress(:),tensor(:,:,:),totd(:),val(:,:),weights(:),          &
   x_coords(:),y_coords(:)
!-----------------------input and initialisation--------------------------
 
 CALL CPU_TIME(start_time)
 OPEN(10,FILE=input_file) 
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,np_types 
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ALLOCATE(nf(nodof,nn),points(nip,ndim),weights(nip),g_coord(ndim,nn),    &
   x_coords(nxe+1),y_coords(nye+1),num(nod),dee(nst,nst),g_g(ndof,nels),  &
   prop(nprops,np_types),etype(nels),evpt(nst,nip,nels),stress(nst),      &
   tensor(nst,nip,nels),coord(nod,ndim),jac(ndim,ndim),der(ndim,nod),     &
   deriv(ndim,nod),g_num(nod,nels),bee(nst,ndof),km(ndof,ndof),eld(ndof), &
   eps(nst),sigma(nst),bload(ndof),eload(ndof),erate(nst),evp(nst),       &
   devp(nst),g(ndof),m1(nst,nst),m2(nst,nst),m3(nst,nst),flow(nst,nst))
 READ(10,*)prop 
 etype=1 
 IF(np_types>1)READ(10,*)etype
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
 WRITE(11,'(2(A,i7))')                                                    &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
 CALL sample(element,points,weights) 
 kv=zero
!-----------------------element stiffness integration and assembly--------
 elements_2: DO iel=1,nels
   ddt=d4*(one+prop(3,etype(iel)))/(d3*prop(2,etype(iel)))
   IF(ddt<dt)dt=ddt 
   CALL deemat(dee,prop(2,etype(iel)),prop(3,etype(iel)))
   num=g_num(:,iel) 
   coord=TRANSPOSE(g_coord(:,num)) 
   g=g_g(:,iel) 
   km=zero
   gauss_pts_1: DO i=1,nip
     CALL shape_der(der,points,i)
     jac=MATMUL(der,coord) 
     det=determinant(jac) 
     CALL invert(jac)
     deriv=MATMUL(jac,der) 
     CALL beemat(bee,deriv)
     km=km+MATMUL(MATMUL(TRANSPOSE(bee),dee),bee)*det*weights(i)
   END DO gauss_pts_1 
   CALL fsparv(kv,km,g,kdiag)
 END DO elements_2                                                            
!-----------------------read load weightings and factorise equations------
 READ(10,*)loaded_nodes 
 ALLOCATE(node(loaded_nodes),val(loaded_nodes,ndim))
 READ(10,*)(node(i),val(i,:),i=1,loaded_nodes) 
 CALL sparin(kv,kdiag)
!-----------------------load increment loop-------------------------------
 READ(10,*)tol,limit,incs 
 ALLOCATE(qinc(incs)) 
 READ(10,*)qinc
 WRITE(11,'(/A)')"  step   load        disp      iters"
 oldis=zero 
 totd=zero 
 tensor=zero 
 ptot=zero
 load_incs: DO iy=1,incs
   ptot=ptot+qinc(iy) 
   iters=0 
   bdylds=zero 
   evpt=zero
!-----------------------plastic iteration loop----------------------------
   its: DO
     iters=iters+1 
     loads=zero
     WRITE(*,'(A,F8.2,A,I4)')"  load",ptot,"  iteration",iters
     DO i=1,loaded_nodes 
       loads(nf(:,node(i)))=val(i,:)*qinc(iy) 
     END DO
     loads=loads+bdylds 
     CALL spabac(kv,loads,kdiag)
!-----------------------check plastic convergence-------------------------
     CALL checon(loads,oldis,tol,converged) 
     IF(iters==1)converged=.FALSE.
     IF(converged.OR.iters==limit)bdylds=zero
!-----------------------go round the Gauss Points ------------------------
     elements_3: DO iel=1,nels
       CALL deemat(dee,prop(2,etype(iel)),prop(3,etype(iel)))
       num=g_num(:,iel) 
       coord=TRANSPOSE(g_coord(:,num))
       g=g_g(:,iel) 
       eld=loads(g) 
       bload=zero
       gauss_pts_2: DO i=1,nip
         CALL shape_der(der,points,i)
         jac=MATMUL(der,coord) 
         det=determinant(jac) 
         CALL invert(jac)
         deriv=MATMUL(jac,der) 
         CALL beemat(bee,deriv)
         eps=MATMUL(bee,eld) 
         eps=eps-evpt(:,i,iel)
         sigma=MATMUL(dee,eps) 
         stress=sigma+tensor(:,i,iel) 
         CALL invar(stress,sigm,dsbar,lode_theta)
!-----------------------check whether yield is violated-------------------
         f=dsbar-SQRT(d3)*prop(1,etype(iel))
         IF(converged.OR.iters==limit)THEN 
           devp=stress 
         ELSE
           IF(f>=zero)THEN
             dq1=zero 
             dq2=d3/two/dsbar 
             dq3=zero
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
!-----------------------update the Gauss Point stresses-------------------
         IF(converged.OR.iters==limit)tensor(:,i,iel)=stress
       END DO gauss_pts_2
!-----------------------compute the total bodyloads vector----------------
       bdylds(g)=bdylds(g)+bload 
       bdylds(0)=zero
     END DO elements_3 
     IF(converged.OR.iters==limit)EXIT
   END DO its 
   totd=totd+loads 
   WRITE(11,'(I5,2E12.4,I5)')iy,ptot,totd(nf(2,node(1))),iters
   IF(iters==limit)EXIT
 END DO load_incs
 CALL dismsh(totd,nf,0.05_iwp,g_coord,g_num,argv,nlen,13)
 CALL vecmsh(totd,nf,0.05_iwp,0.1_iwp,g_coord,g_num,argv,nlen,14)
 CALL CPU_TIME(end_time)
 WRITE(11,'(A,F12.4)')" Time taken = ",end_time-start_time

END SUBROUTINE p61
