SUBROUTINE p83(input_file,output_file)
!-------------------------------------------------------------------------
! Program 8.3 One dimensional consolidation analysis using 2-node "rod"
!             elements. Explicit time integration. Element-by-element.
!             Lumped mass.
!-------------------------------------------------------------------------
 USE main
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::fixed_freedoms,i,iel,j,nels,neq,nlen,nod=2,npri,nprops=1,       &
   np_types,nres,nstep,ntime
 REAL(iwp)::at,a0,dtim,one=1.0_iwp,pt5=0.5_iwp,time,two=2.0_iwp,          &
   zero=0.0_iwp
 CHARACTER(LEN=15)::argv
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),node(:),num(:)
 REAL(iwp),ALLOCATABLE::ell(:),globma(:),kc(:,:),loads(:),mass(:),mm(:,:),&
   newlo(:),press(:),prop(:,:),store_mm(:,:,:),value(:)  
!-----------------------input and initialisation--------------------------
 
 OPEN(10,FILE=input_file) 
 OPEN(11,FILE=output_file)
 READ(10,*)nels,np_types
 neq=nels+1
 ALLOCATE(num(nod),etype(nels),kc(nod,nod),mm(nod,nod),press(0:neq),      &
   prop(nprops,np_types),ell(nels),loads(0:neq),newlo(0:neq),mass(nod),   &
   globma(0:neq),store_mm(nod,nod,nels))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)ell,dtim,nstep,npri,nres,ntime
 globma=zero
 WRITE(11,'(2(A,I5))')" There are",neq," equations"
!-----------------------global conductivity and "mass" matrix assembly----
 elements_1: DO iel=1,nels
   num=(/iel,iel+1/)
   CALL rod_km(kc,prop(1,etype(iel)),ell(iel))
   mm=zero
   DO i=1,nod
     mm(i,i)=ell(iel)/two
     mass(i)=ell(iel)/two
   END DO
   store_mm(:,:,iel)=mm-kc*dtim
   globma(num)=globma(num)+mass
 END DO elements_1
!-----------------------specify initial and boundary values---------------
 READ(10,*)loads(1:)
 loads(0)=zero
 READ(10,*)fixed_freedoms
 globma(1:)=one/globma(1:)
 IF(fixed_freedoms/=0)then
   ALLOCATE(node(fixed_freedoms),value(fixed_freedoms))
   READ(10,*)(node(i),value(i),i=1,fixed_freedoms)
 END IF
!-----------------------time stepping loop--------------------------------
 WRITE(11,'(/A,I3,A)')"    Time         Uav     Pressure (node",nres,")"
 WRITE(11,'(3E12.4)')0.0,0.0,loads(nres)
 a0=zero
 DO iel=1,nels
   a0=a0+pt5*ell(iel)*(loads(iel)+loads(iel+1))
 END DO
 timesteps: DO j=1,nstep
   time=j*dtim
   newlo=zero
   elements_2: DO iel=1,nels  
     num=(/iel,iel+1/)
     mm=store_mm(:,:,iel)
     newlo(num)=newlo(num)+MATMUL(mm,loads(num))
   END DO elements_2
   newlo(0)=zero
   loads=newlo*globma
   IF(fixed_freedoms/=0)loads(node)=value
   at=zero
   DO iel=1,nels
     at=at+pt5*ell(iel)*(loads(iel)+loads(iel+1))
   END DO
   IF(j==ntime)press(1:)=loads(1:)
   IF(j/npri*npri==j)WRITE(11,'(3E12.4)')time,(a0-at)/a0,loads(nres)
 END DO timesteps
 WRITE(11,'(/A,E10.4,A)')"    Depth     Pressure (time=",ntime*dtim,")"
 WRITE(11,'(3E12.4)')0.0,press(1)
 WRITE(11,'(2E12.4)')(SUM(ell(1:i)),press(i+1),i=1,nels)

END SUBROUTINE p83













