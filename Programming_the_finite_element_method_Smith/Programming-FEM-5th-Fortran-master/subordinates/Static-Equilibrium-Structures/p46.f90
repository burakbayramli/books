SUBROUTINE p46(input_file,output_file)
!-------------------------------------------------------------------------
! Program 4.6 Stability (buckling) analysis of elastic beams using 2-node
!             beam elements (elastic foundation optional).
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,iel,iters,k,limit,ndof=4,nels,neq,nod=2,nodof=2,nn,nprops,    &
   np_types,nr,nlen
 REAL(iwp)::eval,tol,zero=0.0_iwp
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),kdiag(:),nf(:,:),num(:) 
 REAL(iwp),ALLOCATABLE::ell(:),evec(:),kg(:,:),gv(:),km(:,:),kv(:),       &
   mm(:,:),prop(:,:)
 CHARACTER(LEN=15)::argv
!-----------------------input and initialisation--------------------------
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nels,nprops,np_types
 nn=nels+1
 ALLOCATE(nf(nodof,nn),ell(nels),num(nod),g(ndof),g_g(ndof,nels),         &
   etype(nels),prop(nprops,np_types),km(ndof,ndof),kg(ndof,ndof),         &
   mm(ndof,ndof))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)ell
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr),limit,tol
 CALL formnf(nf)
 neq=MAXVAL(nf)  
 ALLOCATE(kdiag(neq),evec(0:neq))
!-----------------------loop the elements to find global array sizes------
 kdiag=0
 elements_1: DO iel=1,nels
   num=(/iel,iel+1/)
   CALL num_to_g(num,nf,g)
   g_g(:,iel)=g
   CALL fkdiag(kdiag,g)
 END DO elements_1
 DO i=2,neq
   kdiag(i)=kdiag(i)+kdiag(i-1)
 END DO
 ALLOCATE(kv(kdiag(neq)),gv(kdiag(neq)))
 WRITE(11,'(2(A,I5))')                                                    &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
!-----------------------global stiffness and geometric matrix assembly----
 kv=zero   
 gv=zero
 elements_2: DO iel=1, nels
   mm=zero
   CALL beam_km(km,prop(1,etype(iel)),ell(iel))
   IF(nprops>1)CALL beam_mm(mm,prop(2,etype(iel)),ell(iel))
   CALL beam_gm(kg,ell(iel))
   g=g_g(:,iel)
   CALL fsparv(kv,km+mm,g,kdiag)
   CALL fsparv(gv,kg,g,kdiag)
 END DO elements_2
!-----------------------solve eigenvalue problems-------------------------
 CALL stability(kv,gv,kdiag,tol,limit,iters,evec,eval)
 WRITE(11,'(/A,E12.4,/)')" The buckling load =",eval
 evec(0)=zero
 WRITE(11,'(A)')" The buckling mode"
 WRITE(11,'(/A)')"  Node Translation Rotation"
 DO k=1,nn
   WRITE(11,'(I5,2E12.4)')k,evec(nf(:,k))
 END DO    
 WRITE(11,'(/A,I5,A)')" Converged in",iters," iterations"
 CALL beamdis(evec,nf,0.40_iwp,10,nels,ell,argv,nlen,12)
END SUBROUTINE p46

