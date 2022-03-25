SUBROUTINE p82(input_file,output_file)
!-------------------------------------------------------------------------
! Program 8.2 One dimensional consolidation analysis
! (settlement and excess pore pressure) using 2-node "line" elements. 
! Implicit time integration using the "theta" method.
!-------------------------------------------------------------------------
 USE main
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file 
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,j,nels,neq,nlen,nod=2,npri,nprops=2,       &
   np_types,nres,nstep,ntime
 REAL(iwp)::at,a0,dtim,gamw,penalty=1.0e20_iwp,pt5=0.5_iwp,sc,sl,theta,   &
   time,uav,uavs,zero=0.0_iwp
 CHARACTER(LEN=15)::argv
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),kdiag(:),node(:),num(:)
 REAL(iwp),ALLOCATABLE::bp(:),ell(:),kc(:,:),kv(:),loads(:),mm(:,:),      &
   newlo(:),press(:),prop(:,:),storbp(:),value(:)  
!-----------------------input and initialisation--------------------------
 
 OPEN(10,FILE=input_file) 
 OPEN(11,FILE=output_file)
 READ(10,*)nels,np_types
 neq=nels+1
 ALLOCATE(num(nod),etype(nels),kc(nod,nod),mm(nod,nod),press(0:neq),      &
   prop(nprops,np_types),ell(nels),kdiag(neq),loads(0:neq),newlo(0:neq))
 READ(10,*)prop,gamw  ! prop(1,:)=k, prop(2,:)=mv
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)ell,dtim,nstep,theta,npri,nres,ntime
 kdiag=0
!-----------------------loop the elements to find global arrays sizes-----
 elements_1: DO iel=1,nels
   num=(/iel,iel+1/)
   CALL fkdiag(kdiag,num)
 END DO elements_1
 DO i=2,neq
   kdiag(i)=kdiag(i)+kdiag(i-1)
 END DO
 ALLOCATE(kv(kdiag(neq)),bp(kdiag(neq)))
 bp=zero
 kv=zero 
 WRITE(11,'(2(A,I5))')                                                    &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
!-----------------------global conductivity and "mass" matrix assembly----
 elements_2: DO iel=1,nels
   num=(/iel,iel+1/) 
   CALL rod_km(kc,prop(1,etype(iel))/gamw,ell(iel)) 
   CALL rod_mm(mm,ell(iel)*prop(2,etype(iel)))
   CALL fsparv(kv,kc,num,kdiag)
   CALL fsparv(bp,mm,num,kdiag)
 END DO elements_2
 kv=kv*theta*dtim
 bp=bp+kv
 kv=bp-kv/theta 
!-----------------------specify initial and boundary values---------------
 READ(10,*)loads(1:)
 loads(0)=zero
 a0=zero
 sc=zero
 DO iel=1,nels
   a0=a0+pt5*ell(iel)*(loads(iel)+loads(iel+1))
   sc=sc+pt5*ell(iel)*prop(2,etype(iel))*(loads(iel)+loads(iel+1))
 END DO
 READ(10,*)fixed_freedoms
 IF(fixed_freedoms/=0)then
   ALLOCATE(node(fixed_freedoms),value(fixed_freedoms),                   &
            storbp(fixed_freedoms))
   READ(10,*)(node(i),value(i),i=1,fixed_freedoms)
   bp(kdiag(node))=bp(kdiag(node))+penalty
   storbp=bp(kdiag(node))
 END IF
!-----------------------factorise equations-------------------------------
 CALL sparin(bp,kdiag)                    
!-----------------------time stepping loop--------------------------------
 WRITE(11,'(/2A,I4,A)')"    Time         U_av       U_av_s",              &
   "    Settlement  Pressure (node",nres,")"
 WRITE(11,'(6E12.4)')0.0,0.0,0.0,0.0,loads(nres)
 timesteps: DO j=1,nstep
   time=j*dtim
   CALL linmul_sky(kv,loads,newlo,kdiag)
   IF(fixed_freedoms/=0)newlo(node)=storbp*value
   CALL spabac(bp,newlo,kdiag)
   loads=newlo
   at=zero
   sl=zero
   DO iel=1,nels
     at=at+pt5*ell(iel)*(loads(iel)+loads(iel+1))
     sl=sl+pt5*ell(iel)*prop(2,etype(iel))*(loads(iel)+loads(iel+1))
   END DO
   uav=(a0-at)/a0
   uavs=(sc-sl)/sc
   IF(j==ntime)press(1:)=loads(1:)
   IF(j/npri*npri==j)WRITE(11,'(6E12.4)')time,uav,uavs,sc-sl,loads(nres)
 END DO timesteps
 WRITE(11,'(/A,E10.4,A)')"    Depth     Pressure (time=",ntime*dtim,")"
 WRITE(11,'(3E12.4)')0.0,press(1)
 WRITE(11,'(2E12.4)')(SUM(ell(1:i)),press(i+1),i=1,nels)

END SUBROUTINE p82


