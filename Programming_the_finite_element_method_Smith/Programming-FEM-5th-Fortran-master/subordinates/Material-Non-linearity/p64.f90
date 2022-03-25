SUBROUTINE p64(input_file,output_file)
!-------------------------------------------------------------------------
! Program 6.4 Plane strain slope stability analysis of an elastic-plastic
!             (Mohr-Coulomb) material using 8-node rectangular
!             quadrilaterals. Viscoplastic strain method.
!-------------------------------------------------------------------------
 USE main 
 USE geom 
  IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file

 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,iel,iters,iy,limit,ndim=2,ndof=16,nels,neq,nip=4,nlen,nn,     &
   nod=8,nodof=2,nprops=6,np_types,nsrf,nst=4,nx1,nx2,nye,ny1,ny2
 REAL(iwp)::cf,ddt,det,dq1,dq2,dq3,dsbar,dt=1.0e15_iwp,d4=4.0_iwp,        &
   d180=180.0_iwp,e,f,fmax,h1,h2,lode_theta,one=1.0_iwp,phi,phif,pi,psi,  &
   psif,sigm,snph,start_dt=1.e15_iwp,s1,tnph,tnps,tol,two=2.0_iwp,v,w1,w2,&
   zero=0.0_iwp 
 CHARACTER(LEN=15)::argv,element='quadrilateral' 
 LOGICAL::converged       
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),kdiag(:),nf(:,:), &
   num(:)    
 REAL(iwp),ALLOCATABLE::bdylds(:),bee(:,:),bload(:),coord(:,:),dee(:,:),  &
   devp(:),elastic(:),eld(:),eload(:),eps(:),erate(:),evp(:),evpt(:,:,:), &
   flow(:,:),fun(:),gravlo(:),g_coord(:,:),km(:,:),kv(:),loads(:),m1(:,:),&
   m2(:,:),m3(:,:),oldis(:),points(:,:),prop(:,:),sigma(:),srf(:),        &
   weights(:)
!-----------------------input and initialisation--------------------------

 OPEN(10,FILE=input_file) 
 OPEN(11,FILE=output_file)
 READ(10,*)w1,s1,w2,h1,h2,nx1,nx2,ny1,ny2,np_types 
 nye=ny1+ny2
 nels=nx1*nye+ny2*nx2 
 nn=(3*nye+2)*nx1+2*nye+1+(3*ny2+2)*nx2
 ALLOCATE(nf(nodof,nn),points(nip,ndim),weights(nip),g_coord(ndim,nn),    &
   num(nod),dee(nst,nst),evpt(nst,nip,nels),coord(nod,ndim),fun(nod),     &
   g_g(ndof,nels),g_num(nod,nels),bee(nst,ndof),km(ndof,ndof),eld(ndof),  &
   eps(nst),sigma(nst),bload(ndof),eload(ndof),erate(nst),evp(nst),       &
   devp(nst),g(ndof),m1(nst,nst),m2(nst,nst),m3(nst,nst),flow(nst,nst),   &
   prop(nprops,np_types),etype(nels))
 READ(10,*)prop 
 etype=1 
 IF(np_types>1)READ(10,*)etype
 CALL emb_2d_bc(nx1,nx2,ny1,ny2,nf) 
 neq=MAXVAL(nf)    
 ALLOCATE(kdiag(neq),loads(0:neq),bdylds(0:neq),oldis(0:neq),             &
   gravlo(0:neq),elastic(0:neq)) 
   kdiag=0
!-----------------------loop the elements to find global arrays sizes-----
 elements_1: DO iel=1,nels
   CALL emb_2d_geom(iel,nx1,nx2,ny1,ny2,w1,s1,w2,h1,h2,coord,num)
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
 gravlo=zero 
!-----------------------element stiffness integration and assembly--------
 elements_2: DO iel=1,nels
   CALL deemat(dee,prop(5,etype(iel)),prop(6,etype(iel))) 
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num)) 
   g=g_g(:,iel) 
   km=zero 
   eld=zero
   gauss_pts_1: DO i=1,nip
     CALL shape_fun(fun,points,i)
     CALL bee8(bee,coord,points(i,1),points(i,2),det)
     km=km+MATMUL(MATMUL(TRANSPOSE(bee),dee),bee)*det*weights(i)
     eld(nodof:ndof:nodof)=eld(nodof:ndof:nodof)+fun(:)*det*weights(i)
   END DO gauss_pts_1
   CALL fsparv(kv,km,g,kdiag) 
   gravlo(g)=gravlo(g)-eld*prop(4,etype(iel))
 END DO elements_2
!-----------------------factorise equations-------------------------------
 CALL sparin(kv,kdiag) 
 pi=ACOS(-one)
!-----------------------trial strength reduction factor loop--------------
 READ(10,*)tol,limit,nsrf 
 ALLOCATE(srf(nsrf)) 
 READ(10,*)srf
 WRITE(11,'(/A)')"    srf    max disp  iters"
 srf_trials: DO iy=1,nsrf
   dt=start_dt
   DO i=1,np_types
     phi=prop(1,i) 
     tnph=TAN(phi*pi/d180) 
     phif=ATAN(tnph/srf(iy)) 
     snph=SIN(phif) 
     e=prop(5,i) 
     v=prop(6,i)
     ddt=d4*(one+v)*(one-two*v)/(e*(one-two*v+snph**2)) 
     IF(ddt<dt)dt=ddt
   END DO 
   iters=0 
   bdylds=zero 
   evpt=zero 
   oldis=zero
!-----------------------plastic iteration loop----------------------------
   its: DO
     fmax=zero 
     iters=iters+1 
     loads=gravlo+bdylds 
     CALL spabac(kv,loads,kdiag) 
     loads(0)=zero
     IF(iy==1.AND.iters==1)elastic=loads
!-----------------------check plastic convergence-------------------------
     CALL checon(loads,oldis,tol,converged) 
     IF(iters==1)converged=.FALSE.
     IF(converged.OR.iters==limit)bdylds=zero
!-----------------------go round the Gauss Points ------------------------
     elements_3: DO iel=1,nels
       bload=zero 
       phi=prop(1,etype(iel)) 
       tnph=TAN(phi*pi/d180)
       phif=ATAN(tnph/srf(iy))*d180/pi 
       psi=prop(3,etype(iel))
       tnps=TAN(psi*pi/d180) 
       psif=ATAN(tnps/srf(iy))*d180/pi
       cf=prop(2,etype(iel))/srf(iy) 
       e=prop(5,etype(iel)) 
       v=prop(6,etype(iel)) 
       CALL deemat(dee,e,v) 
       num=g_num(:,iel)
       coord=TRANSPOSE(g_coord(:,num)) 
       g=g_g(:,iel) 
       eld=loads(g)
       gauss_pts_2: DO i=1,nip
         CALL bee8(bee,coord,points(i,1),points(i,2),det)
         eps=MATMUL(bee,eld) 
         eps=eps-evpt(:,i,iel) 
         sigma=MATMUL(dee,eps)
         CALL invar(sigma,sigm,dsbar,lode_theta)
!-----------------------check whether yield is violated-------------------
         CALL mocouf(phif,cf,sigm,dsbar,lode_theta,f) 
         IF(f>fmax)fmax=f
         IF(converged.OR.iters==limit)THEN 
           devp=sigma 
         ELSE
           IF(f>=zero.OR.(converged.OR.iters==limit))THEN
             CALL mocouq(psif,dsbar,lode_theta,dq1,dq2,dq3)
             CALL formm(sigma,m1,m2,m3) 
             flow=f*(m1*dq1+m2*dq2+m3*dq3)
             erate=MATMUL(flow,sigma) 
             evp=erate*dt
             evpt(:,i,iel)=evpt(:,i,iel)+evp 
             devp=MATMUL(dee,evp)
           END IF
         END IF
         IF(f>=zero)THEN
           eload=MATMUL(devp,bee) 
           bload=bload+eload*det*weights(i)
         END IF
       END DO gauss_pts_2
!-----------------------compute the total bodyloads vector----------------
       bdylds(g)=bdylds(g)+bload 
       bdylds(0)=zero
     END DO elements_3
     WRITE(*,'(A,F7.2,A,I4,A,F8.3)')                                      &
       "  srf",srf(iy),"  iteration",iters,"  F_max",fmax
     IF(converged.OR.iters==limit)EXIT
   END DO its 
   WRITE(11,'(F7.2,E12.4,I5)')srf(iy),MAXVAL(ABS(loads)),iters 
   IF(iters==limit)EXIT
 END DO srf_trials
 CALL dismsh(loads-elastic,nf,0.1_iwp,g_coord,g_num,argv,nlen,13)
 CALL vecmsh(loads-elastic,nf,0.1_iwp,0.25_iwp,g_coord,g_num,argv,nlen,14)

END SUBROUTINE p64
