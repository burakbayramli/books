SUBROUTINE p73(input_file,output_file) 
!-------------------------------------------------------------------------
! SUBROUTINE 7.3 Analysis of plane free-surface flow using 4-node
!             quadrilaterals. "Analytical" form of element conductivity
!             matrix.
!-------------------------------------------------------------------------
 USE main; 
 USE geom; 
 IMPLICIT NONE 
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_down,fixed_seep,fixed_up,i,iel,iters,k,limit,nci,ndim=2,  &
   nels,neq,nlen,nod=4,nn,nxe,nye,np_types; 
   CHARACTER(LEN=15)::argv
 REAL(iwp)::d180=180.0_iwp,hdown,hup,initial_height,one=1.0_iwp,          &
   penalty=1.e20_iwp,tol,zero=0.0_iwp; 
 LOGICAL::converged
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g_num(:,:),kdiag(:),node_down(:),          &
   node_seep(:),node_up(:),num(:) 
 REAL(iwp),ALLOCATABLE::angs(:),bottom_width(:),coord(:,:),disps(:),      &
   g_coord(:,:),kay(:,:),kp(:,:),kv(:),kvh(:),loads(:),oldpot(:),         &
   prop(:,:),surf(:),top_width(:)
!-----------------------input and initialisation--------------------------
 
 OPEN(10,FILE=input_file); 
 OPEN(11,FILE=output_file)
 READ(10,*)nxe,nye,tol,limit,np_types; 
 nels=nxe*nye 
 nn=(nxe+1)*(nels/nxe+1); 
 neq=nn
 ALLOCATE(g_coord(ndim,nn),coord(nod,ndim),bottom_width(nxe+1),           &
   top_width(nxe+1),surf(nxe+1),angs(nxe+1),kp(nod,nod),num(nod),         &
   g_num(nod,nels),prop(ndim,np_types),kdiag(neq),kay(ndim,ndim),         &
   etype(nels),loads(0:neq),disps(0:neq),oldpot(0:neq))
 READ(10,*)prop; 
 etype=1; 
 IF(np_types>1)READ(10,*)etype
 READ(10,*)bottom_width; 
 READ(10,*)top_width; 
 READ(10,*)initial_height
 surf=initial_height 
 angs=ATAN(surf/(top_width-bottom_width))*d180/acos(-one)
 READ(10,*)hup,fixed_up; 
 ALLOCATE(node_up(fixed_up)); 
 READ(10,*)node_up
 READ(10,*)hdown,fixed_down; 
 ALLOCATE(node_down(fixed_down)) 
 READ(10,*)node_down; 
 fixed_seep=nels/nxe-fixed_down 
 ALLOCATE(node_seep(fixed_seep))
 DO i=1,fixed_seep; 
   node_seep(i)=i*(nxe+1)+1; 
 END DO; 
 kdiag=0
!-----------------------loop the elements to find global arrays sizes-----
 elements_1: DO iel=1,nels
   CALL geom_freesurf(iel,nxe,fixed_seep,fixed_down,                      &
     hdown,bottom_width,angs,surf,coord,num); 
   CALL fkdiag(kdiag,num)
 END DO elements_1
 DO i=2,neq; 
   kdiag(i)=kdiag(i)+kdiag(i-1); 
 END DO
 ALLOCATE(kv(kdiag(neq)),kvh(kdiag(neq)))
 WRITE(11,'(2(A,I5))')                                                    &
   "There are ",neq,"  equations and the skyline storage is ",kdiag(neq)
!-----------------------global conductivity matrix assembly---------------
 oldpot=zero; 
 iters=0
 its: DO
   iters=iters+1; 
   kv=zero
   elements_2: DO iel=1,nels
     kay=zero; 
     DO i=1,ndim; 
       kay(i,i)=prop(i,etype(iel)); 
     END DO
     CALL geom_freesurf(iel,nxe,fixed_seep,fixed_down,                    &
       hdown,bottom_width,angs,surf,coord,num)
     g_num(:,iel)=num; 
     g_coord(:,num)=TRANSPOSE(coord) 
     CALL seep4(kp,coord,kay); 
     CALL fsparv(kv,kp,num,kdiag)
   END DO elements_2; 
   kvh=kv
!-----------------------specify boundary values---------------------------
   loads=zero; 
   kv(kdiag(node_up))=kv(kdiag(node_up))+penalty
   loads(node_up)=kv(kdiag(node_up))*hup
   kv(kdiag(node_down))=kv(kdiag(node_down))+penalty
   loads(node_down)=kv(kdiag(node_down))*hdown
   kv(kdiag(node_seep))=kv(kdiag(node_seep))+penalty
   DO i=1,fixed_seep
     loads(node_seep(i))=kv(kdiag(node_seep(i)))*                         &
       (hdown+(surf(1)-hdown)*(fixed_seep+1-i)/(fixed_seep+1))
   END DO
!-----------------------equation solution---------------------------------
   CALL sparin(kv,kdiag); 
   CALL spabac(kv,loads,kdiag)  
   surf(1:nxe)=loads(1:nxe)
!-----------------------check convergence---------------------------------
   CALL checon(loads,oldpot,tol,converged) 
   IF(converged.OR.iters==limit)EXIT
 END DO its; 
 CALL linmul_sky(kvh,loads,disps,kdiag)
 WRITE(11,'(/A)')"  Node Total Head  Flow rate"
 DO k=1,nn; 
   WRITE(11,'(I5,2E12.4)')k,loads(k),disps(k); 
 END DO 
 disps(0)=zero; 
 WRITE(11,'(/A)')"       Inflow      Outflow"
 WRITE(11,'(5X,2E12.4)')                                                  &
    SUM(disps,MASK=disps<zero),SUM(disps,MASK=disps>zero)
 WRITE(11,'(/A,I3,A)')" Converged in",iters," iterations"   
 CALL mesh(g_coord,g_num,argv,nlen,12) 
 READ(10,*)nci; 
 CALL contour(loads,g_coord,g_num,nci,argv,nlen,13)

END SUBROUTINE p73
