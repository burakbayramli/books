SUBROUTINE p69(input_file,output_file)
!-------------------------------------------------------------------------
! Program 6.9 Plane strain construction of an elastic-plastic
!             (Mohr-Coulomb) embankment in layers on a foundation using
!             8-node quadrilaterals. Viscoplastic strain method.
!-------------------------------------------------------------------------
 USE main
 USE geom
  IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file

 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::enxe,enye,fnxe,fnye,i,iel,ii,incs,iters,itype,iy,k,lifts,limit, &
   lnn,ndim=2,ndof=16,nels,neq,newele,nip=4,nlen,nn,nod=8,nodof=2,nr,     &
   nst=4,oldele,oldnn
 REAL(iwp)::c,c_e,c_f,det,dq1,dq2,dq3,dsbar,dt,d4=4.0_iwp,d180=180.0_iwp, &
   e,e_e,e_f,f,gamma,gama_e,gama_f,k0,lode_theta,one=1.0_iwp,phi,phi_e,   &
   phi_f,pi,psi,psi_e,psi_f,sigm,snph,tol,two=2.0_iwp,v,v_e,v_f,          &
   zero=0.0_iwp
 CHARACTER(LEN=15)::argv,element='quadrilateral'
 LOGICAL::converged   
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),kdiag(:),lnf(:,:),&
   nf(:,:),num(:)  
 REAL(iwp),ALLOCATABLE::bdylds(:),bee(:,:),bload(:),coord(:,:),dee(:,:),  &
   der(:,:),deriv(:,:),devp(:),edepth(:),eld(:),eload(:),eps(:),erate(:), &
   evp(:),evpt(:,:,:),ewidth(:),fdepth(:),flow(:,:),fun(:),fwidth(:),     &
   gc(:),gravlo(:),g_coord(:,:),jac(:,:),km(:,:),kv(:),loads(:),m1(:,:),  &
   m2(:,:),m3(:,:),oldis(:),points(:,:),sigma(:),stress(:),tensor(:,:,:), &
   totd(:),weights(:)
!-----------------------input and initialisation--------------------------

 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)fnxe,fnye,nn,incs,limit,tol,lifts,enxe,enye,itype,k0,e_f,v_f,  &
   c_f,phi_f,psi_f,gama_f,e_e,v_e,c_e,phi_e,psi_e,gama_e
!-----------------------calculate the total number of elements------------
 k=0
 DO i=1,enye-1
   k=i+k
 END DO
 nels=fnxe*fnye+(enxe*enye-k)
 ALLOCATE(nf(nodof,nn),points(nip,ndim),weights(nip),g_coord(ndim,nn),    &
   edepth(enye+1),num(nod),dee(nst,nst),evpt(nst,nip,nels),ewidth(enxe+1),&
   coord(nod,ndim),fun(nod),etype(nels),g_g(ndof,nels),jac(ndim,ndim),    &
   der(ndim,nod),deriv(ndim,nod),g_num(nod,nels),bee(nst,ndof),           &
   km(ndof,ndof),eld(ndof),eps(nst),sigma(nst),bload(ndof),eload(ndof),   &
   erate(nst),evp(nst),devp(nst),g(ndof),m1(nst,nst),m2(nst,nst),         &
   m3(nst,nst),flow(nst,nst),stress(nst),fwidth(fnxe+1),fdepth(fnye+1),   &
   gc(ndim),tensor(nst,nip,nels))
 READ(10,*)fwidth,fdepth,ewidth,edepth
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)
 WRITE(11,'(A,I5)')" The final number of elements is:",nels
 WRITE(11,'(A,I5)')" The final number of freedoms is:",neq
!-----------------------set the element type------------------------------
 etype(1:fnxe*fnye)=1
 etype(fnxe*fnye+1:nels)=2 
!-----------set up the global node numbers and element nodal coordinates--
 CALL fmglem(fnxe,fnye,enxe,g_num,lifts)
 CALL fmcoem(g_num,g_coord,fwidth,fdepth,ewidth,edepth,                   &
   enxe,lifts,fnxe,fnye,itype)
 ALLOCATE(totd(0:neq))
 tensor=zero
 totd=zero
 pi=ACOS(-one)
!-----------------------loop the elements to find the global g------------
 elements_1: DO iel=1,nels
   num=g_num(:,iel)
   CALL num_to_g(num,nf,g)
   g_g(:,iel)=g
 END DO elements_1
 CALL sample(element,points,weights)
!-----------------------construct another lift----------------------------
 lift_number: DO ii=1,lifts
   WRITE(11,'(/A,I5)')" Lift number",ii
!-----------------------calculate how many elements there are-------------
   IF(ii<=lifts)THEN
     IF(ii==1)THEN
       newele=fnxe*fnye
       oldele=newele
     ELSE
       newele=enxe-(ii-2)
       oldele=oldele+newele
     END IF
!-----------------------calculate how many nodes there are----------------
     IF(ii==1)THEN
       lnn=(fnxe*2+1)*(fnye+1)+(fnxe+1)*fnye
       oldnn=lnn
     END IF
     IF(ii>1)THEN
       lnn=oldnn+(enxe-(ii-2))*2+1+(enxe-(ii-2)+1)
       oldnn=lnn
     END IF
     ALLOCATE(lnf(nodof,lnn))
     lnf=nf(:,1:lnn)
!-----------------------recalculate the number of freedoms neq------------
     neq=MAXVAL(lnf)
     ALLOCATE(kdiag(neq))
     kdiag=0
!-----------------------loop the elements to find global arrays sizes-----
     elements_2: DO iel=1,oldele
       g=g_g(:,iel)
       CALL fkdiag(kdiag,g)
     END DO elements_2
     DO i=2,neq
       kdiag(i)=kdiag(i)+kdiag(i-1)
     END DO
     WRITE(11,'(3(A,I5))')" There are",neq," freedoms"
     WRITE(11,'(3(A,I5))')" There are",oldele," elements after",          &
       newele," were added"
   END IF
   ALLOCATE(kv(kdiag(neq)),loads(0:neq),bdylds(0:neq),oldis(0:neq),       &
     gravlo(0:neq))
     gravlo=zero
     loads=zero
     kv=zero
!-----------------------element stiffness integration and assembly-------- 
   elements_3: DO iel=1,oldele
     IF(etype(iel)==1)THEN
       gamma=gama_f
       e=e_f
       v=v_f
     ELSE
       gamma=gama_e
       e=e_e
       v=v_e
     END IF
     IF(iel<=(oldele-newele))gamma=zero
     num=g_num(:,iel) 
     coord=TRANSPOSE(g_coord(:,num))
     g=g_g(:,iel)
     km=zero 
     CALL deemat(dee,e,v)
     eld=zero
     gauss_pts_1: DO i=1,nip
       CALL shape_fun(fun,points,i)
       gc=MATMUL(fun,coord)
!-----------------------initial stress in foundation----------------------
       IF(ii==1)THEN
         tensor(2,i,iel)=-one*(fdepth(fnye+1)-gc(2))*gamma
         tensor(1,i,iel)=k0*tensor(2,i,iel)
         tensor(4,i,iel)=tensor(1,i,iel)
         tensor(3,i,iel)=zero
       END IF
       CALL shape_der(der,points,i) 
       jac=MATMUL(der,coord)
       det=determinant(jac)
       CALL invert(jac)
       deriv=MATMUL(jac,der)
       CALL beemat(bee,deriv)
       km=km+MATMUL(MATMUL(TRANSPOSE(bee),dee),bee)*det*weights(i)
       DO k=2,ndof,2
         eld(k)=eld(k)+fun(k/2)*det*weights(i)
       END DO
     END DO gauss_pts_1
     CALL fsparv(kv,km,g,kdiag) 
     IF(ii<=lifts)gravlo(g)=gravlo(g)-eld*gamma
     gravlo(0)=zero
   END DO elements_3
!-----------------------factorise equations--and-factor gravlo by incs----
   CALL sparin(kv,kdiag)
   gravlo=gravlo/incs
!-----------------------apply gravity loads incrementally-----------------
   load_incs: DO iy=1,incs
     iters=0
     oldis=zero
     bdylds=zero
     evpt(:,:,1:oldele)=zero
!-----------------------iteration loop------------------------------------
     its: DO
       iters=iters+1
       WRITE(*,'(A,I3,A,I3,A,I4)')"  lift",ii,"  increment",iy,           &
         "  iteration",iters
       loads=zero
       loads=gravlo+bdylds
       CALL spabac(kv,loads,kdiag)
!-----------------------check convergence---------------------------------
       CALL checon(loads,oldis,tol,converged)
       IF(iters==1)converged=.FALSE.
       IF(converged.OR.iters==limit)bdylds=zero
!-----------------------go round the Gauss Points-------------------------
       elements_4: DO iel=1,oldele
         IF(etype(iel)==1)THEN
           phi=phi_f
           c=c_f
           e=e_f
           v=v_f
           psi=psi_f
         ELSE
           phi=phi_e
           c=c_e
           e=e_e
           v=v_e
           psi=psi_e
         END IF
         snph=SIN(phi*pi/d180) 
         dt=d4*(one+v)*(one-two*v)/(e*(one-two*v+snph**2))
         CALL deemat(dee,e,v)
         bload=zero
         num=g_num(:,iel)
         coord=TRANSPOSE(g_coord(:,num))
         g=g_g(:,iel)
         eld=loads(g)
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
           IF(ii==1)THEN
             stress=tensor(:,i,iel)
           ELSE
             stress=tensor(:,i,iel)+sigma
           END IF
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
           IF(f>=zero)THEN
             eload=MATMUL(devp,bee)
             bload=bload+eload*det*weights(i)
           END IF
!-----------------------if apropriate update the Gauss point stresses----
           IF(converged.OR.iters==limit)THEN 
             IF(ii/=1)tensor(:,i,iel)=stress
           END IF
         END DO gauss_pts_2
!-----------------------compute the total bodyloads vector----------------
         bdylds(g)=bdylds(g)+bload
         bdylds(0)=zero
       END DO elements_4
       IF(converged.OR.iters==limit)EXIT
     END DO its
     IF(ii/=1)totd(:neq)=totd(:neq)+loads(:neq)
     WRITE(11,'(2(A,I5),A)')" Increment",iy," took ",iters,               &
       " iterations to converge"
     IF(iy==incs.OR.iters==limit)WRITE(11,'(A,E12.4)')                    &
       " Max displacement is",MAXVAL(ABS(loads))
     IF(iters==limit)THEN
       CALL dismsh(loads,lnf,0.05_iwp,g_coord,g_num,argv,nlen,13)
       CALL vecmsh(loads,lnf,0.05_iwp,0.1_iwp,g_coord,g_num,argv,nlen,14)
       
     END IF
   END DO load_incs
   DEALLOCATE(lnf,kdiag,kv,loads,bdylds,oldis,gravlo)
 END DO lift_number

END SUBROUTINE p69
