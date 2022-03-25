SUBROUTINE p66(input_file,output_file)
!-------------------------------------------------------------------------
! Program 6.6 Plane strain bearing capacity analysis of an elastic-plastic
!             (von Mises) material using 8-node rectangular quadrilaterals. 
!             Flexible smooth footing. Load control. 
!             Consistent tangent stiffness. 
!             Closest Point Projection Method (CPPM).
!-------------------------------------------------------------------------
 USE main
 USE geom
  IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file

 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,iel,incs,iters,iy,k,limit,loaded_nodes,ndim=2,ndof=16,nels,   &
   neq,nip=4,nlen,nn,nod=8,nodof=2,nprops=3,np_types,nst=4,nxe,nye
 REAL(iwp)::bot,det,dlam,dsbar,dslam,d3=3.0_iwp,end_time,fnew,lode_theta, &
   ltol,one=1.0_iwp,ptot,sigm,start_time,tol,zero=0.0_iwp
 CHARACTER(LEN=15)::argv,element='quadrilateral'
 LOGICAL::converged
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),kdiag(:),nf(:,:), &
   node(:),num(:)
 REAL(iwp),ALLOCATABLE::acat(:,:),bee(:,:),bdylds(:),caflow(:),&
   coord(:,:),daatd(:,:),ddylds(:),dee(:,:),der(:,:),deriv(:,:),dload(:), &
   dsigma(:),eld(:),eload(:),eps(:),g_coord(:,:),jac(:,:),km(:,:),kv(:),  &
   loads(:),loadsr(:),points(:,:),prop(:,:),qinc(:),qinva(:),qinvr(:),    &
   qmat(:,:),ress(:),rmat(:,:),sigma(:),stress(:),tensor(:,:,:),          &
   tensorl(:,:,:),totd(:),totdl(:),trial(:),val(:,:),vmfl(:),vmfla(:),    &
   vmflq(:),weights(:),x_coords(:),y_coords(:)
!-----------------------input and initialisation--------------------------

 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 CALL CPU_TIME(start_time)
 READ(10,*)nxe,nye,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ALLOCATE(nf(nodof,nn),points(nip,ndim),weights(nip),g_coord(ndim,nn),    &
   x_coords(nxe+1),y_coords(nye+1),num(nod),dee(nst,nst),                 &
   tensor(nst,nip,nels),g_g(ndof,nels),coord(nod,ndim),jac(ndim,ndim),    &
   der(ndim,nod),deriv(ndim,nod),g_num(nod,nels),bee(nst,ndof),           &
   km(ndof,ndof),eld(ndof),eps(nst),sigma(nst),eload(ndof),g(ndof),       &
   vmfl(nst),qinvr(nst),stress(nst),dload(ndof),caflow(nst),dsigma(nst),  &
   ress(nst),rmat(nst,nst),acat(nst,nst),qmat(nst,nst),qinva(nst),        &
   daatd(nst,nst),vmflq(nst),vmfla(nst),prop(nprops,np_types),etype(nels),&
   tensorl(nst,nip,nels),trial(nst))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)x_coords,y_coords
 CALL bc_rect(nxe,nye,nf,'y')
 neq=MAXVAL(nf)
 ALLOCATE(kdiag(neq),loads(0:neq),bdylds(0:neq),totd(0:neq),ddylds(0:neq),&
   loadsr(0:neq),totdl(0:neq))
   kdiag=0
!-----------------------loop the elements to find global arrays sizes-----
 elements_1: DO iel=1,nels
   CALL geom_rect(element,iel,x_coords,y_coords,coord,num,'y')
   CALL num_to_g(num,nf,g)
   CALL fkdiag(kdiag,g)
   g_num(:,iel)=num
   g_coord(:,num)=TRANSPOSE(coord)
   g_g(:,iel)=g
 END DO elements_1
 CALL mesh(g_coord,g_num,argv,nlen,12)
 DO i=2,neq
   kdiag(i)=kdiag(i)+kdiag(i-1)
 END DO
 ALLOCATE(kv(kdiag(neq)))
 WRITE(11,'(2(A,I7))')                                                    &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
 kv=zero
 totd=zero
 tensor=zero
 tensorl=zero
 CALL sample(element,points,weights)
!--------------starting element stiffness integration and assembly--------
 elements_2: DO iel=1,nels
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
!-------------------------------------------------------------------------
 READ(10,*)ltol,tol,limit,incs
 ALLOCATE(qinc(incs))
 READ(10,*)qinc
 ptot=zero
 WRITE(11,'(/A)')"  step   load        disp      iters"
 load_increments: DO iy=1,incs
   ptot=ptot+qinc(iy)
   totdl=zero
   loads=zero
   loadsr=zero   
   bdylds=zero
   iters=0
!-----------------------load increment loop-------------------------------
   DO i=1,loaded_nodes
     loads(nf(:,node(i)))=val(i,:)*qinc(iy)   ! load increment
     loadsr(nf(:,node(i)))=val(i,:)*ptot      ! total load
   END DO
!-----------------------plastic iteration loop----------------------------
   iterations: DO
     iters=iters+1
     WRITE(*,'(A,F8.2,A,I4)')"  load",ptot,"  iteration",iters
     IF(iters/=1)loads=zero
     loads=loads+bdylds
     CALL spabac(kv,loads,kdiag)
     loads(0)=zero
     totdl=totdl+loads
     ddylds=zero
     kv=zero
!-----------------------go round the Gauss Points ------------------------   
     elements_3: DO iel=1,nels
       dload=zero
       num=g_num(:,iel)
       coord=TRANSPOSE(g_coord(:,num))
       g=g_g(:,iel)
       km=zero
       eld=totdl(g)
       gauss_pts_2: DO i=1,nip
         CALL shape_der(der,points,i)
         jac=MATMUL(der,coord)
         det=determinant(jac)
         CALL invert(jac)
         deriv=MATMUL(jac,der) 
         CALL beemat(bee,deriv)
         eps=MATMUL(bee,eld) 
         CALL deemat(dee,prop(2,etype(iel)),prop(3,etype(iel)))
         sigma=MATMUL(dee,eps)
         stress=sigma+tensorl(:,i,iel)
         trial=stress
         CALL invar(stress,sigm,dsbar,lode_theta)
!-----------------------check whether yield is violated-------------------
         fnew=dsbar-SQRT(d3)*prop(1,etype(iel))
         IF(fnew>=zero)THEN
           dlam=zero
           iterate_on_fnew: DO
             CALL vmflow(stress,dsbar,vmfl)
             caflow=MATMUL(dee,vmfl)       
             ress=stress-trial+caflow*dlam
             CALL fmacat(vmfl,acat)  
             acat=acat/dsbar
             qmat=dlam*MATMUL(dee,acat)    
             DO k=1,4
               qmat(k,k)=qmat(k,k)+one
             END DO
             CALL invert(qmat)
             vmflq=MATMUL(vmfl,qmat)
             vmfla=MATMUL(vmflq,dee)       
             dslam=(fnew-DOT_PRODUCT(vmflq,ress))/DOT_PRODUCT(vmfla,vmfl)
             dsigma=-MATMUL(qmat,ress)-MATMUL(MATMUL(qmat,dee),vmfl)*dslam
             stress=stress+dsigma
             CALL invar(stress,sigm,dsbar,lode_theta)
             fnew=dsbar-SQRT(d3)*prop(1,etype(iel))
             dlam=dlam+dslam
             IF(fnew<ltol)EXIT
           END DO iterate_on_fnew
           CALL vmflow(stress,dsbar,vmfl)
           CALL fmrmat(vmfl,dsbar,dlam,dee,rmat)
           caflow=MATMUL(rmat,vmfl)
           bot=DOT_PRODUCT(vmfl,caflow)
           CALL formaa(vmfl,rmat,daatd)
           dee=rmat-daatd/bot
         END IF
         km=km+MATMUL(MATMUL(TRANSPOSE(bee),dee),bee)*det*weights(i)
!-----------------------update the Gauss Point stresses-------------------
         tensor(:,i,iel)=stress
         eload=MATMUL(stress,bee)
         dload=dload+eload*det*weights(i)
       END DO gauss_pts_2
!-----------------------compute the total bodyloads vector----------------
       ddylds(g)=ddylds(g)+dload
       ddylds(0)=zero
       CALL fsparv(kv,km,g,kdiag)
     END DO elements_3
     CALL sparin(kv,kdiag)
     bdylds=loadsr-ddylds
     bdylds(0)=zero
     IF(norm(bdylds(1:))/norm(loadsr(1:))<tol)converged=.TRUE.
     IF(iters==1)converged=.FALSE.
     IF(converged.OR.iters==limit)EXIT
   END DO iterations
   tensorl=tensor
   totd=totd+totdl
   WRITE(11,'(I5,2E12.4,I5)')iy,ptot,totd(nf(2,node(1))),iters
   IF(iters==limit)EXIT
 END DO load_increments
 CALL dismsh(totd,nf,0.05_iwp,g_coord,g_num,argv,nlen,13)
 CALL vecmsh(totd,nf,0.05_iwp,0.1_iwp,g_coord,g_num,argv,nlen,14)
 CALL CPU_TIME(end_time)
 WRITE(11,'(A,F12.4)')" Time taken = ",end_time-start_time

END SUBROUTINE p66
