SUBROUTINE p88(input_file,output_file)
!-------------------------------------------------------------------------
! Program 8.8 General two- (plane) or three-dimensional analysis of the
!             consolidation equation. Implicit time integration using
!             the "theta" method.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file    
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,j,nci,ndim,nels,neq,nip,nlen,nn,nod=4,npri,&
   np_types,nres,nstep,ntime
 CHARACTER(len=15)::argv,element
 REAL(iwp)::det,dtim,penalty=1.0e20_iwp,theta,time,zero=0.0_iwp   
!----------------------------- dynamic arrays-----------------------------
 INTEGER,ALLOCATABLE::etype(:),g_num(:,:),node(:),num(:),kdiag(:)
 REAL(iwp),ALLOCATABLE::bp(:),coord(:,:),der(:,:),deriv(:,:),fun(:),      &
   g_coord(:,:),jac(:,:),kay(:,:),kc(:,:),kv(:),loads(:),newlo(:),        &
   ntn(:,:),mm(:,:),points(:,:),prop(:,:),storbp(:),value(:),weights(:)
!-----------------------input and initialisation--------------------------
 
 OPEN(10,FILE=input_file) 
 OPEN(11,FILE=output_file)
 READ(10,*)element,nels,nn,nip,nod,ndim,np_types
 neq=nn
 ALLOCATE(points(nip,ndim),g_coord(ndim,nn),coord(nod,ndim),fun(nod),     &
   etype(nels),jac(ndim,ndim),weights(nip),num(nod),ntn(nod,nod),         &
   g_num(nod,nels),der(ndim,nod),deriv(ndim,nod),kc(nod,nod),mm(nod,nod), &
   kay(ndim,ndim),kdiag(neq),prop(ndim,np_types),newlo(0:neq),loads(0:neq))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)g_coord
 READ(10,*)g_num
 READ(10,*)dtim,nstep,theta,npri,nres
 IF(ndim==2.AND.nod==4)THEN
   READ(10,*)ntime,nci
   CALL mesh(g_coord,g_num,argv,nlen,12)
 END IF
 kdiag=0
!-----------loop the elements to set up global geometry and kdiag --------
 elements_1: DO iel=1,nels
   num=g_num(:,iel)
   CALL fkdiag(kdiag,num)
 END DO elements_1
 DO i=2,neq
   kdiag(i)=kdiag(i)+kdiag(i-1)
 END DO
 WRITE(11,'(2(a,i5))')                                                    &
 " There are",neq,"  equations and the skyline storage is ",kdiag(neq)
 ALLOCATE(kv(kdiag(neq)),bp(kdiag(neq)))
 CALL sample(element,points,weights)
 kv=zero
 bp=zero
!------------- global conductivity matrix assembly------------------------
 elements_2: DO iel=1,nels
   kay=zero
   DO i=1,ndim
     kay(i,i)=prop(i,etype(iel))
   END DO
   num=g_num(:,iel)
   coord=TRANSPOSE(g_coord(:,num))
   kc=zero
   mm=zero
   gauss_pts: DO i=1,nip
     CALL shape_der(der,points,i)
     CALL shape_fun(fun,points,i)
     jac=MATMUL(der,coord)
     det=determinant(jac)
     CALL invert(jac)
     deriv=MATMUL(jac,der)
     kc=kc+MATMUL(MATMUL(TRANSPOSE(deriv),kay),deriv)*det*weights(i)
     CALL cross_product(fun,fun,ntn)
     mm=mm+ntn*det*weights(i)
   END DO gauss_pts 
   CALL fsparv(kv,kc,num,kdiag)
   CALL fsparv(bp,mm,num,kdiag)
 END DO elements_2
 kv=kv*theta*dtim
 bp=bp+kv
 kv=bp-kv/theta 
!---------------initial and boundary conditions data----------------------
 READ(10,*)loads(1:)
 loads(0)=zero
 READ(10,*)fixed_freedoms
 IF(fixed_freedoms/=0)then
   ALLOCATE(node(fixed_freedoms),value(fixed_freedoms),                   &
            storbp(fixed_freedoms))
   READ(10,*)(node(i),value(i),i=1,fixed_freedoms)
   bp(kdiag(node))=bp(kdiag(node))+penalty
   storbp=bp(kdiag(node))
 END IF
!------------------------factorise left hand side-------------------------
 CALL sparin(bp,kdiag)                    
!-------------------time stepping recursion-------------------------------
 WRITE(11,'(/a,i3,a)')"    Time      Pressure (node",nres,")"
 WRITE(11,'(2e12.4)')0.0,loads(nres)
 timesteps: DO j=1,nstep
   time=j*dtim
   CALL linmul_sky(kv,loads,newlo,kdiag)
   IF(fixed_freedoms/=0)newlo(node)=storbp*value 
   CALL spabac(bp,newlo,kdiag)
   loads=newlo  
   IF(ndim==2.AND.nod==4.AND.j==ntime)                                    &
     CALL contour(loads,g_coord,g_num,nci,argv,nlen,13)
   IF(j/npri*npri==j)WRITE(11,'(2e12.4)')time,loads(nres)
 END DO timesteps

END SUBROUTINE p88
