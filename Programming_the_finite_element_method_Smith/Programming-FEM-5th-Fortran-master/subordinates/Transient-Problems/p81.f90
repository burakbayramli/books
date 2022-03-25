SUBROUTINE p81(input_file,output_file)
!-------------------------------------------------------------------------
! Program 8.1 One dimensional consolidation analysis using 2-node "rod"
!             elements. Implicit time integration using the "theta" method.
!-------------------------------------------------------------------------
 USE main
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,j,nels,neq,nlen,nod=2,npri,nprops=1,       &
   np_types,nres,nstep,ntime
 REAL(iwp)::at,a0,dtim,penalty=1.0e20_iwp,pt5=0.5_iwp,theta,time,         &
   zero=0.0_iwp
 CHARACTER(LEN=15)::argv
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),node(:),num(:),kdiag(:)
 REAL(iwp),ALLOCATABLE::bp(:),ell(:),kc(:,:),kv(:),loads(:),newlo(:),     &
   mm(:,:),press(:),prop(:,:),storbp(:),value(:)  
!-----------------------input and initialisation--------------------------
 
 OPEN(10,FILE=input_file) 
 OPEN(11,FILE=output_file)
 READ(10,*)nels,np_types
 neq=nels+1
 ALLOCATE(num(nod),etype(nels),kc(nod,nod),mm(nod,nod),press(0:neq),      &
   prop(nprops,np_types),ell(nels),kdiag(neq),loads(0:neq),newlo(0:neq))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)ell  
 READ(10,*)dtim,nstep,theta,npri,nres,ntime
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
 WRITE(11,'(2(a,i5))')                                                    &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
!-----------------------global conductivity and "mass" matrix assembly----
 elements_2: DO iel=1,nels
 num=(/iel,iel+1/)
   CALL rod_km(kc,prop(1,etype(iel)),ell(iel))
   CALL rod_mm(mm,ell(iel)) 
   CALL fsparv(kv,kc,num,kdiag)
   CALL fsparv(bp,mm,num,kdiag)
 END DO elements_2
 kv=kv*theta*dtim
 bp=bp+kv
 kv=bp-kv/theta 
!-----------------------specify initial and boundary values---------------
 loads(0)=zero
 READ(10,*)loads(1:)
 a0=zero
 DO iel=1,nels
   a0=a0+pt5*ell(iel)*(loads(iel)+loads(iel+1))
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
 WRITE(11,'(/a,i3,a)')"    Time         Uav      Pressure (node",nres,")"
 WRITE(11,'(3e12.4)')0.0,0.0,loads(nres)
 timesteps: DO j=1,nstep
   time=j*dtim
   CALL linmul_sky(kv,loads,newlo,kdiag)
   IF(fixed_freedoms/=0)newlo(node)=storbp*value 
   CALL spabac(bp,newlo,kdiag)
   loads=newlo
   at=zero
   DO iel=1,nels
     at=at+pt5*ell(iel)*(loads(iel)+loads(iel+1))
   END DO
   IF(j==ntime)press(1:)=loads(1:)
   IF(j/npri*npri==j)WRITE(11,'(3e12.4)')time,(a0-at)/a0,loads(nres)
 END DO timesteps
 WRITE(11,'(/a,e10.4,a)')"    Depth     Pressure (time=",ntime*dtim,")"
 WRITE(11,'(3e12.4)')0.0,press(1)
 WRITE(11,'(2e12.4)')(SUM(ell(1:i)),press(i+1),i=1,nels)

END SUBROUTINE p81
