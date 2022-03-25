SUBROUTINE p68(input_file,output_file)
!-------------------------------------------------------------------------
! Program p68 Plane strain bearing capacity analysis of an elastic-plastic
!             (von Mises) material using 8-node rectangular quadrilaterals. 
!             Flexible smooth footing. Load control.
!             Consistent tangent stiffness. 
!             Radial Return Method (RR). Line search technique.
!-------------------------------------------------------------------------
 USE main
 USE geom
  IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file

 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,j,iel,incs,jj,iters,iy,limit,loaded_nodes,ndim=2,ndof=16,nels,&
   neq,nip=4,nlen,nn,nod=8,nodof=2,nprops=3,np_types,nst=4,nxe,nye
 REAL(iwp)::bot,con1,con2,det,dsbar,d2=2.0_iwp,d3=3.0_iwp,end_time,fnew,  &
   length,lode_theta,one=1.0_iwp,ptot,pt5=0.5_iwp,sigm,start_time,sx,sy,  &
   sz,s0,s1,tol,zero=0.0_iwp
 CHARACTER(LEN=15)::argv,element='quadrilateral'
 LOGICAL::converged
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),kdiag(:),nf(:,:), &
   node(:),num(:)
 REAL(iwp),ALLOCATABLE::bee(:,:),bdylds(:),bdylds0(:),coord(:,:),         &
   ddylds(:),dee(:,:),deep(:,:),der(:,:),deriv(:,:),dload(:),eld(:),      &
   eload(:),eps(:),g_coord(:,:),jac(:,:),km(:,:),kv(:),loads(:),loadsr(:),&
   points(:,:),prop(:,:),qinc(:),sigma(:),stress(:),tensor(:,:,:),        &
   tensorl(:,:,:),totd(:),totdl(:),totdll(:),totdlo(:),val(:,:),          &
   weights(:),x_coords(:),y_coords(:)
!-----------------------input and initialisation--------------------------

 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 CALL cpu_time(start_time)
 READ(10,*)nxe,nye,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ALLOCATE(nf(nodof,nn),points(nip,ndim),weights(nip),g_coord(ndim,nn),    &
   x_coords(nxe+1),y_coords(nye+1),num(nod),dee(nst,nst),deep(nst,nst),   &
   dload(ndof),tensor(nst,nip,nels),g_g(ndof,nels),coord(nod,ndim),       &
   jac(ndim,ndim),der(ndim,nod),deriv(ndim,nod),g_num(nod,nels),          &
   bee(nst,ndof),km(ndof,ndof),eld(ndof),eps(nst),sigma(nst),eload(ndof), &
   g(ndof),stress(nst),prop(nprops,np_types),etype(nels),                 &
   tensorl(nst,nip,nels))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)x_coords,y_coords
 CALL bc_rect(nxe,nye,nf,'y')
 neq=MAXVAL(nf)
 ALLOCATE(kdiag(neq),loads(0:neq),bdylds(0:neq),totd(0:neq),ddylds(0:neq),&
   loadsr(0:neq),totdl(0:neq),totdlo(0:neq),bdylds0(0:neq),totdll(0:neq))
!-----------------------loop the elements to find global arrays sizes-----
 kdiag=0
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
     CALL beemat (bee,deriv)
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
 ptot=zero
 bdylds=zero
 WRITE(11,'(/A)')"  step   load        disp      iters"
 load_increments: DO iy=1,incs
   ptot=ptot+qinc(iy)
   bdylds=zero
   loads=zero
   loadsr=zero
   totdl=zero
   DO i=1,loaded_nodes
     loads(nf(:,node(i)))=val(i,:)*qinc(iy)
     loadsr(nf(:,node(i)))=val(i,:)*ptot
   END DO
   iters=0
!-----------------------plastic iteration loop----------------------------
   iterations: DO
     iters=iters+1
     WRITE(*,'(A,F8.2,A,I4)')"  load",ptot,"  iteration",iters
     IF(iters/=1)loads=zero
     loads=loads+bdylds
     bdylds0=loads
     CALL spabac(kv,loads,kdiag)
     loads(0)=zero
     length=one
     totdlo=loads
     totdll=totdl
     s0=DOT_PRODUCT(bdylds0,totdlo)
     line_search: DO jj=1,2
       totdl=totdll+length*totdlo
       bdylds=zero
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
         CALL deemat(dee,prop(2,etype(iel)),prop(3,etype(iel)))
         bot=prop(2,etype(iel))/(one+prop(3,etype(iel)))
         gauss_pts_2: DO i=1,nip
           CALL shape_der(der,points,i)
           jac=MATMUL(der,coord)
           det=determinant(jac)
           CALL invert(jac)
           deriv=MATMUL(jac,der)
           CALL beemat(bee,deriv)
           eps=MATMUL(bee,eld)
           sigma=MATMUL(dee,eps)
           stress=sigma+tensorl(:,i,iel) 
           CALL invar(stress,sigm,dsbar,lode_theta)
!-----------------------check whether yield is violated-------------------
           fnew=dsbar-SQRT(d3)*prop(1,etype(iel))
           deep=dee
           IF(fnew>=zero)THEN
             con1=fnew/dsbar
             con2=d3/d2*(one-con1)/dsbar**2*bot
             sx=stress(1)-sigm
             sy=stress(2)-sigm
             sz=stress(4)-sigm
             deep(1,1)=dee(1,1)-con2*sx*sx-con1*d2/d3*bot
             deep(1,2)=dee(1,2)-con2*sx*sy+con1/d3*bot
             deep(1,3)=dee(1,3)-con2*stress(3)*sx
             deep(1,4)=dee(1,4)-con2*sx*sz+con1/d3*bot
             deep(2,2)=dee(2,2)-con2*sy*sy-con1*d2/d3*bot
             deep(2,3)=dee(2,3)-con2*stress(3)*sy
             deep(2,4)=dee(2,4)-con2*sy*sz+con1/d3*bot
             deep(3,3)=dee(3,3)-con2*stress(3)**2-con1/d2*bot
             deep(3,4)=dee(3,4)-con2*stress(3)*sz
             deep(4,4)=dee(4,4)-con2*sz*sz-con1*d2/d3*bot
             DO j=1,3
               deep(j+1:4,j)=deep(j,j+1:4)
             END DO
             stress(1)=stress(1)-con1*sx
             stress(2)=stress(2)-con1*sy 
             stress(4)=stress(4)-con1*sz
             stress(3)=stress(3)-con1*stress(3) 
           END IF
           km=km+MATMUL(MATMUL(TRANSPOSE(bee),deep),bee)*det*weights(i)
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
       s1=DOT_PRODUCT(bdylds,totdlo)
       IF(ABS(s1)<=pt5*ABS(s0))EXIT
       IF(s0/s1>zero)THEN
         length=pt5*s0/s1
       ELSE
         length=s0/s1*pt5+SQRT((s0/s1*pt5)**2-s0/s1)
       ENDIF
     END DO line_search
     IF(iters==1)converged=.FALSE.
     IF(iters/=1.AND.norm(bdylds(1:))/norm(loadsr(1:))<tol)converged=.TRUE.
     IF(converged.OR.iters==limit)EXIT
   END DO iterations
   tensorl=tensor
   totd=totd+totdl
   WRITE(11,'(I5,2E12.4,I5)')iy,ptot,totd(nf(2,node(1))),iters
   IF(iters==limit)EXIT
 END DO load_increments
 CALL dismsh(totd,nf,0.05_iwp,g_coord,g_num,argv,nlen,13)
 CALL vecmsh(totd,nf,0.05_iwp,0.1_iwp,g_coord,g_num,argv,nlen,14)
 CALL cpu_time(end_time)
 WRITE(11,'(A,F12.4)')" Time taken = ",end_time-start_time

END SUBROUTINE p68
