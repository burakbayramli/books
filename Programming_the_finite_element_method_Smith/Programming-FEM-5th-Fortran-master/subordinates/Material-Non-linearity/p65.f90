SUBROUTINE p65(input_file,output_file)
!-------------------------------------------------------------------------
! Program 6.5 Plane strain earth pressure analysis of an elastic-plastic
!             (Mohr-Coulomb) material using 8-node rectangular
!             quadrilaterals. Initial stress method.
!-------------------------------------------------------------------------
 USE main
 USE geom
  IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file

 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,incs,iters,iy,k,limit,ndim=2,ndof=16,nels, &
   neq,nip=4,nlen,nn,nod=8,nodof=2,nprops=7,np_types,nr,nst=4,nxe,nye 
 REAL(iwp)::c,det,dsbar,e,f,fac,fnew,gamma,k0,lode_theta,one=1.0_iwp,ot,  &
   pav,phi,psi,pr,presc,pt5=0.5_iwp,sigm,penalty=1.0e20_iwp,tol,v,          &
   zero=0.0_iwp
 CHARACTER(LEN=15)::argv,element='quadrilateral'
 LOGICAL::converged       
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),g_num(:,:),kdiag(:),nf(:,:), &
   no(:),node(:),num(:),sense(:)    
 REAL(iwp),ALLOCATABLE::bdylds(:),bee(:,:),bload(:),coord(:,:),dee(:,:),  &
   der(:,:),deriv(:,:),eld(:),eload(:),elso(:),eps(:),fun(:),gc(:),       &
   g_coord(:,:),jac(:,:),km(:,:),kv(:),loads(:),oldis(:),pl(:,:),         &
   points(:,:),prop(:,:),react(:),rload(:),sigma(:),storkv(:),stress(:),  &
   tensor(:,:,:),totd(:),weights(:),x_coords(:),y_coords(:)
!-----------------------input and initialisation--------------------------

 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,np_types
 CALL mesh_size(element,nod,nels,nn,nxe,nye)
 ALLOCATE(nf(nodof,nn),points(nip,ndim),weights(nip),g_coord(ndim,nn),    &
   x_coords(nxe+1),y_coords(nye+1),num(nod),dee(nst,nst),fun(nod),        &
   tensor(nst,nip,nels),g_g(ndof,nels),coord(nod,ndim),stress(nst),       &
   jac(ndim,ndim),der(ndim,nod),deriv(ndim,nod),g_num(nod,nels),          &
   bee(nst,ndof),km(ndof,ndof),eld(ndof),eps(nst),sigma(nst),bload(ndof), &
   eload(ndof),pl(nst,nst),elso(nst),g(ndof),gc(ndim),rload(ndof),        &
   prop(nprops,np_types),etype(nels))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)x_coords,y_coords
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)     
 ALLOCATE(kdiag(neq),loads(0:neq),bdylds(0:neq),oldis(0:neq),totd(0:neq), &
   react(0:neq))
   kdiag=0
!-----------------------loop the elements to find global arrays sizes-----
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
 CALL sample(element,points,weights)
 tensor=zero
 kv=zero
!-----------------------element stiffness integration and assembly--------
 elements_2: DO iel=1,nels
   CALL deemat(dee,prop(6,etype(iel)),prop(7,etype(iel)))
   gamma=prop(4,etype(iel))
   k0=prop(5,etype(iel))
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   g=g_g(:,iel)
   km=zero
   gauss_pts_1: DO i=1,nip
     CALL shape_fun(fun,points,i)
     gc=MATMUL(fun,coord)
     tensor(2,i,iel)=(gc(2)-y_coords(1))*gamma
     tensor(1,i,iel)=(gc(2)-y_coords(1))*gamma*k0
     tensor(4,i,iel)=tensor(1,i,iel)
     CALL shape_der(der,points,i)
     jac=MATMUL(der,coord)
     det=determinant(jac)
     CALL invert(jac)
     deriv=MATMUL(jac,der)
     CALL beemat(bee,deriv)
     km=km+MATMUL(matmul(transpose(bee),dee),bee)*det*weights(i)
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
!-----------------------displacement increment loop-----------------------
 READ(10,*)tol,limit,incs,presc
 WRITE(11,'(/A)')                                                         &
   "  step   disp      load(av)    load(react)   moment    iters"
 oldis=zero
 totd=zero
 bdylds=zero
 disp_incs: DO iy=1,incs      
   iters=0
   react=zero
!-----------------------plastic iteration loop----------------------------
   its: DO
     iters=iters+1
     loads=bdylds
     WRITE(*,'(A,E11.3,A,I4)')"  disp",iy*presc,"  iteration",iters
     DO i=1,fixed_freedoms
       loads(nf(1,node(i)))=storkv(i)*presc
     END DO
     CALL spabac(kv,loads,kdiag)
     bdylds=zero
!-----------------------check plastic convergence-------------------------
     CALL checon(loads,oldis,tol,converged)
     IF(iters==1)converged=.FALSE.
!-----------------------go round the Gauss Points ------------------------
     elements_3: DO iel=1,nels
       phi=prop(1,etype(iel))
       c=prop(2,etype(iel))
       psi=prop(3,etype(iel))
       e=prop(6,etype(iel))
       v=prop(7,etype(iel))
       CALL deemat(dee,e,v)
       bload=zero
       rload=zero
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
         sigma=MATMUL(dee,eps)
         stress=sigma+tensor(:,i,iel)
         CALL invar(stress,sigm,dsbar,lode_theta)
!-----------------------check whether yield is violated-------------------
         CALL mocouf(phi,c,sigm,dsbar,lode_theta,fnew)
         elso=zero
         IF(fnew>zero)THEN
           stress=tensor(:,i,iel)
           CALL invar(stress,sigm,dsbar,lode_theta)
           CALL mocouf(phi,c,sigm,dsbar,lode_theta,f)
           fac=fnew/(fnew-f)
           stress=(one-fac)*sigma+tensor(:,i,iel)
           CALL mcdpl(phi,psi,dee,stress,pl)
           pl=fac*pl
           elso=MATMUL(pl,eps)
           eload=MATMUL(elso,bee)
           bload=bload+eload*det*weights(i)
         END IF
!-----------------------update the Gauss Point stresses-------------------
         IF(converged.OR.iters==limit)THEN
           tensor(:,i,iel)=tensor(:,i,iel)+sigma-elso
           rload=rload+MATMUL(tensor(:,i,iel),bee)*det*weights(i)
         END IF
       END DO gauss_pts_2
!-----------------------compute the total bodyloads vector ---------------
       bdylds(g)=bdylds(g)+bload
       react(g)=react(g)+rload
       bdylds(0)=zero
       react(0)=zero
     END DO elements_3
     IF(converged.OR.iters==limit)EXIT
   END DO its
   totd=totd+loads
   pr=zero
   ot=zero
   pav=zero
   DO i=1,fixed_freedoms
     pr=pr+react(no(i))
     ot=ot+react(no(i))*g_coord(2,node(i))
   END DO
   DO i=1,4
     pav=pav+(y_coords(i)-y_coords(i+1))*(tensor(1,1,i)+tensor(1,3,i))*pt5
   END DO
   WRITE(11,'(I5,4E12.4,I5)')iy,iy*presc,-pav,pr,ot,iters
   IF(iters==limit)EXIT
 END DO disp_incs
 CALL dismsh(totd,nf,0.05_iwp,g_coord,g_num,argv,nlen,13)
 CALL vecmsh(totd,nf,0.05_iwp,0.5_iwp,g_coord,g_num,argv,nlen,14)

END SUBROUTINE p65
