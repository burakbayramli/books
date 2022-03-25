SUBROUTINE p63(input_file,output_file)
!-------------------------------------------------------------------------
! Program 6.3 Plane strain bearing capacity analysis of an elastic-plastic
!             (Mohr-Coulomb) material using 8-node rectangular
!             quadrilaterals. Rigid smooth footing. Displacement control.
!             Viscoplastic strain method.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file

 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,incs,iters,iy,i3,i4,i5,j,k,limit,nbo2,     &
   ndim=2,ndof=16,nels,neq,nip=4,nlen,nn,nod=8,nodof=2,nprops=6,np_types, &
   nst=4,nxe,nye 
 REAL(iwp)::c,ddt,det,dq1,dq2,dq3,dsbar,dt=1.0e15_iwp,d3=3.0_iwp,         &
   d4=4.0_iwp,d6=6.0_iwp,d180=180.0_iwp,e,f,lode_theta,one=1.0_iwp,pav,   &
   penalty=1.0e20_iwp,phi,pi,pr,presc,psi,qq,qs,sigm,snph,                &
   start_dt=1.e15_iwp,tol,two=2.0_iwp,v,zero=0.0_iwp  
 CHARACTER(LEN=15)::argv,element='quadrilateral'
 LOGICAL::converged
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),kdiag(:),nf(:,:), &
   no(:),node(:),num(:)    
 REAL(iwp),ALLOCATABLE::bdylds(:),bee(:,:),bload(:),coord(:,:),dee(:,:),  &
   der(:,:),deriv(:,:),devp(:),eld(:),eload(:),eps(:),erate(:),evp(:),    &
   evpt(:,:,:),flow(:,:),fun(:),gravlo(:),g_coord(:,:),jac(:,:),km(:,:),  &
   kv(:),kvc(:),loads(:),m1(:,:),m2(:,:),m3(:,:),oldis(:),points(:,:),    &
   prop(:,:),react(:),rload(:),sigma(:),storkv(:),stress(:),tensor(:,:,:),&
   totd(:),weights(:),x_coords(:),y_coords(:)   
!-----------------------input and initialisation--------------------------

 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,nbo2,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ALLOCATE(nf(nodof,nn),points(nip,ndim),weights(nip),g_coord(ndim,nn),    &
   x_coords(nxe+1),y_coords(nye+1),num(nod),dee(nst,nst),g_g(ndof,nels),  &
   prop(nprops,np_types),etype(nels),evpt(nst,nip,nels),stress(nst),      &
   tensor(nst,nip,nels),coord(nod,ndim),jac(ndim,ndim),der(ndim,nod),     &
   deriv(ndim,nod),g_num(nod,nels),bee(nst,ndof),km(ndof,ndof),eld(ndof), &
   eps(nst),sigma(nst),bload(ndof),eload(ndof),erate(nst),evp(nst),       &
   devp(nst),g(ndof),m1(nst,nst),m2(nst,nst),m3(nst,nst),flow(nst,nst),   &
   fun(nod),rload(ndof))
 READ(10,*)prop
 etype=1 
 IF(np_types>1)READ(10,*)((etype(j+(i-1)*nye),i=1,nxe),j=1,nye)
 ! compute the node freedom array in y direction
 CALL bc_rect(nxe,nye,nf,'y')
 neq=MAXVAL(nf) 
 READ(10,*)qs,x_coords,y_coords
 ALLOCATE(kdiag(neq),loads(0:neq),bdylds(0:neq),oldis(0:neq),totd(0:neq), &
   gravlo(0:neq),react(0:neq))
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
 ALLOCATE(kv(kdiag(neq)),kvc(kdiag(neq)))
 WRITE(11,'(2(A,I7))')                                                    &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
 pi=ACOS(-one)
 dt=start_dt 
 DO i=1,np_types
   phi=prop(1,i)
   snph=SIN(phi*pi/d180)
   e=prop(5,i)
   v=prop(6,i) 
   ddt=d4*(one+v)*(one-two*v)/(e*(one-two*v+snph**2))
   IF(ddt<dt)dt=ddt
 END DO 
 CALL sample(element,points,weights)
 gravlo=zero
 kv=zero
!-----------------------element stiffness integration and gravity loads---
 elements_2: DO iel=1,nels
   CALL deemat(dee,prop(5,etype(iel)),prop(6,etype(iel)))
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   g=g_g(:,iel)
   km=zero
   eld=zero
   gauss_pts_2: DO i=1,nip
     CALL shape_fun(fun,points,i)
     CALL shape_der(der,points,i)
     jac=MATMUL(der,coord)
     det=determinant(jac)
     CALL invert(jac)
     deriv=MATMUL(jac,der)
     CALL beemat(bee,deriv)
     km=km+MATMUL(MATMUL(TRANSPOSE(bee),dee),bee)*det*weights(i)
     eld(2:ndof:2)=eld(2:ndof:2)+fun(:)*det*weights(i)
   END DO gauss_pts_2
   CALL fsparv(kv,km,g,kdiag)
   gravlo(g)=gravlo(g)-eld*prop(4,etype(iel))
 END DO elements_2
 kvc=kv 
!-----------------------surcharge loads-----------------------------------
 DO i=1,nxe
   i3=g_num(3,(i-1)*nye+1)
   i4=g_num(4,(i-1)*nye+1)
   i5=g_num(5,(i-1)*nye+1)
   qq=(x_coords(i+1)-x_coords(i))*qs
   gravlo(nf(2,i3))=gravlo(nf(2,i3))-qq/d6
   gravlo(nf(2,i4))=gravlo(nf(2,i4))-qq*two/d3
   gravlo(nf(2,i5))=gravlo(nf(2,i5))-qq/d6
 END DO    
!-----------------------factorise equations-------------------------------
 CALL sparin(kv,kdiag)
 CALL spabac(kv,gravlo,kdiag)
 gravlo(0)=zero
!-----------------------set up initial stresses---------------------------
 elements_3: DO iel=1,nels
   CALL deemat(dee,prop(5,etype(iel)),prop(6,etype(iel)))
   g=g_g(:,iel)
   eld=gravlo(g)
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   int_pts_2: DO i=1,nip
     CALL shape_der(der,points,i)
     jac=MATMUL(der,coord)
     CALL invert(jac)
     deriv=MATMUL(jac,der)
     CALL beemat(bee,deriv)
     sigma=MATMUL(dee,MATMUL(bee,eld))
     tensor(1,i,iel)=sigma(1)
     tensor(2,i,iel)=sigma(2)
     tensor(3,i,iel)=sigma(3)
     tensor(4,i,iel)=sigma(4)
   END DO int_pts_2
 END DO elements_3
!-----------------------fixed displacement data and factorise equations----
 fixed_freedoms=2*nbo2+1
 ALLOCATE(node(fixed_freedoms),no(fixed_freedoms),storkv(fixed_freedoms))
 node(1)=1
 k=1
 DO i=1,nbo2
   k=k+(2*nye)+1
   node(2*i)=k
   k=k+nye+1
   node(2*i+1)=k
 END DO   
 DO i=1,fixed_freedoms
   no(i)=nf(2,node(i))
 END DO
 kv=kvc
 kv(kdiag(no))=kv(kdiag(no))+penalty
 storkv=kv(kdiag(no))
 CALL sparin(kv,kdiag)
!-----------------------load increment loop-------------------------------
 READ(10,*)tol,limit,incs,presc
 oldis=zero
 totd=zero 
 WRITE(11,'(/A)')"  step   disp        load1       load2     iters"
 disp_incs: DO iy=1,incs
   iters=0
   bdylds=zero
   react=zero
   evpt=zero
!-----------------------plastic iteration loop----------------------------
   its: DO
     iters=iters+1
     WRITE(*,'(A,E11.3,A,I4)')"  disp",iy*presc,"  iteration",iters
     loads=zero
     loads=loads+bdylds
     DO i=1,fixed_freedoms
       loads(no(i))=storkv(i)*presc
     END DO
     CALL spabac(kv,loads,kdiag)
!-----------------------check plastic convergence-------------------------
     CALL checon(loads,oldis,tol,converged)
     IF(iters==1)converged=.FALSE.
     IF(converged.OR.iters==limit)bdylds=zero
!-----------------------go round the Gauss Points ------------------------
     elements_4: DO iel=1,nels
       phi=prop(1,etype(iel))
       c=prop(2,etype(iel))
       psi=prop(3,etype(iel)) 
       CALL deemat(dee,prop(5,etype(iel)),prop(6,etype(iel)))
       bload=zero
       rload=zero
       num=g_num(:,iel)
       coord=TRANSPOSE(g_coord(:,num))
       g=g_g(:,iel)
       eld=loads(g)
       gauss_pts_4: DO i=1,nip
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
!-----------------------update the Gauss Point stresses-------------------
         IF(converged.OR.iters==limit)THEN
           tensor(:,i,iel)=stress 
           rload=rload+MATMUL(stress,bee)*det*weights(i)
         END IF
       END DO gauss_pts_4
!-----------------------compute the total bodyloads vector----------------
       bdylds(g)=bdylds(g)+bload
       react(g)=react(g)+rload
       bdylds(0)=zero
     END DO elements_4
     IF(converged.OR.iters==limit)EXIT
   END DO its
   totd=totd+loads
   pr=zero
   DO i=1,fixed_freedoms
     pr=pr+react(no(i))
   END DO
   pr=pr/x_coords(nbo2+1)
   pav=zero
   DO i=1,nbo2 
     pav=pav+tensor(2,1,(i-1)*nye+1)+tensor(2,2,(i-1)*nye+1)
   END DO
   pav=pav/(two*nbo2)
   WRITE(11,'(I5,3E12.4,I5)')iy,-totd(1),-pr,-pav,iters
   IF(iters==limit)EXIT
 END DO disp_incs
 CALL dismsh(totd,nf,0.05_iwp,g_coord,g_num,argv,nlen,13)
 CALL vecmsh(totd,nf,0.05_iwp,0.1_iwp,g_coord,g_num,argv,nlen,14)

END SUBROUTINE p63

